(* Editing text files functionally *)

(* This module offers a way to edit text files in a functional way.
   Simply put, the contents of a set of texts is used to create new
   texts.

     The use of immutable texts to create new texts is called here a_
   functional edit_. An edit combines several _transforms_. A
   transform is a pure function from one text to another, the latter
   being considered a modified version of the former. As a given input
   can be shared amongst several transforms and several transforms can
   target the same output text, we can define edits on multiple inputs
   and multiple outputs.

     The specification of edits is done by means of abstract programs
   whose evaluation on input strings produces output string
   buffers. Those edits can be considered programs of an embedded
   Domain-Specific Language (eDSL). That eDSL is elementary in the
   sense that it features a very small number of instructions, like
   skipping or copying text, writing text etc.

     This module also offers a second eDSL, based on a _filtering
   paradigm_, whereby the implicit action consists in copying the
   incoming stream of characters to the output. This paradigm is found
   in XSLT, for example, and shortens edits by eliding copying actions
   -- unless it is clearer to make them explicit in certain
   contexts. The primitive edits in this filtering eDSL are much more
   numerous than in the elementary eDSL, in order to better express
   complex editing action. They are compiled to the elementary edits
   before being executed. *)

(* External dependencies *)

module Pos = SourceLoc.Pos
module Partition = UnionFind.Partition

(* Transforms *)

module type TRANS =
  sig
    type t
    type trans = t

    val compare   : t -> t -> int
    val equal     : t -> t -> bool
    val to_string : t -> string
  end

module Make (Trans : TRANS) :
 sig
   (* As explained above, inputs and outputs are texts. To denote them
      and use them as keys in maps, we need to refer to them by means
      of unique names, as we would do with files. Those names are
      called here _handles_. *)

   type handle = string

   module HMap : Map.S with type key = handle
   module TSet : Set.S with type elt = Trans.t
   module TMap : Map.S with type key = Trans.t
   module TEq  : Partition.S with type item = Trans.t

   (* I/O Maps *)

   module IO_map :
     sig
       (* We need now to connect transforms and handles.

            The type [binding] describes a two-way mapping between
          handles and transforms, both for input and output.

            * The field [lift] maps a handle to a set of transforms
              (see module [PolySet] in library RedBlackTrees), because
              a handle can be used by multiple transforms, either as
              input or output.

            * Conversely, the field [drop] maps a transform to a
              handle, as each transform is associated to one handle,
              either as input or output. *)

       type binding = {
         lift : TSet.t HMap.t;
         drop : handle TMap.t
       }

       (* The type [t] gathers bindings for input and output. We call
          it here an _I/O map_. *)

       type t = {
         input     : binding;
         output    : binding;
         to_string : t -> string  (* For debug *)
       }

       type io_map = t

       (* The value of [init to_string] is an I/O map initialised with
          empty bindings and the function [to_string] that provides a
          string representing a transform. *)

       val init : (t -> string) -> t

       (* The value of [add (input, trans, output) io] is a copy of
          the I/O map [io] updated with the transform [trans] from its
          input handle [input] to its output handle
          [output]. Transforms are defined on a single input and
          single output, and subsequent calls with the same transform
          will replace any previous definition. *)

       type input  = handle
       type output = handle

       val add : (input * Trans.t * output) -> t -> t

       (* Printing of I/O maps (for debug) *)

       val print_io_map : t -> Buffer.t
     end

   (* Descriptors *)

   (* Handles are lowered to different data structures, depending if
      on the input or the input side. Those mappings are called _I/O
      descriptors_. *)

   module IO_desc :
     sig
       (* Handles are mapped to string buffers of type [Buffer.t]. In
          the case of input, those buffers are coupled with a position
          in them corresponding to the last read character. Note that
          the position is encoded by a value of type [Pos.t], as it
          would be in a file, because we want to be able to express
          where edits occur in terms of lines and columns or
          horizontal offsets. This is enough to resume resume reading
          from where we left. *)

       type in_desc  = (Pos.t * Buffer.t) TMap.t

       (* Contrary to input descriptors, output descriptor do not
          contain a position in the string buffer, because we always
          append to it (so the position is implicitly the last
          character in the buffer). *)

       type out_desc = Buffer.t TMap.t

       (* The type [t] gathers I/O descriptors for inputs and
          outputs. *)

       type t = {in_desc : in_desc; out_desc : out_desc}

       type io_desc = t

     (* An I/O descriptor are made from an I/O map and a partition of
        classes of equivalent transformations. *)

       val mk_desc : IO_map.t -> TEq.partition -> io_desc
     end

   (* A Low-Level DSL for editing *)

   (* The module [Plain] specifies a tiny Domain Specific Language
      (DSL) by means of an abstract syntax whose interpretation
      results in editing possibly multiple buffers, but not
      in-place. Several output buffers may be produced as a result. *)

   module Plain :
     sig
       (* An edit is parameterised by a set of transforms. More
          precisely, the type of an edit should be a sum type whose
          variants denote each a transform. This enables an edit to
          combine multiple transforms. *)

       type t
       type edit = t

       (* Edits of type [t] are denoted by the values of the following
          function calls and constants:

            * [null] is the empty edit, a no-operation;

            * the edit [write trans text edit] denotes the action of
              writing the string [text] into the output of the
              transform [trans], followed by the edit [edit] to be
              applied next;

            * the edit [copy trans ~until:pos edit] represents the
              action of copying the characters in the input of
              transform [trans] until the position [pos] (excluded),
              and writing them to the output of [trans], followed by
              the edit [edit] to be applied next;

            * the edit [skip trans ~until:pos edit] denotes the action
              of skipping the text in the input until the position
              [pos] (excluded) is reached, followed by the edit [edit]
              to be applied next;

            * the edit [find trans char edit] means skipping the text
              in the input until the character [char] (included) is
              reached, followed by the edit [edit] to be applied
              next. Note that this edit, contrary to the others, is
              not static, as it depends on the contents of the input
              text.

          Note how edits can combine multiple transforms by simply
          passing a different transform (first argument) to the
          functions above. *)

       val null  : t
       val write : Trans.t -> string      -> t -> t
       val copy  : Trans.t -> until:Pos.t -> t -> t
       val skip  : Trans.t -> until:Pos.t -> t -> t
       val find  : Trans.t -> char        -> t -> t

       (* The value of the call [check edit] is [true] if, and only
          if, the positions enabling the individual edits are strictly
          increasing for a given transformation, so they can be
          applied in one pass over the corresponding input. If this
          condition is not satisfied, and error reports the first two
          positions that violate the condition. *)

       val check : t -> (unit, Pos.t * Pos.t) Stdlib.result

       (* The call [show ~offsets io edits] prints the edits [edits]
          interpreted with respect to the I/O map [io]. *)

       val show : offsets:bool -> IO_map.t -> edit list -> Buffer.t

       (* Making the I/O descriptors and optionally optimising lists
          of edits by merging them pairwise whenever possible, like a
          peep-hole optimisation. Indeed, edits on the same input can
          be defined separately and this module implements a function
          which tries to merge pairwise edits that are sequentially
          composable, in order to minimise the number of passes on a
          given input source.

            The value of [build ~opt io edits] is a pair whose first
          component is an I/O descriptor, of type ['trans desc]. If
          the call is [build ~opt:true io edits], then the second
          component is a list of edits equivalent to the list [edits],
          but hopefully shorter due to possible mergers between
          contiguous edits within the list. Mergers are
          left-associative, so the list of edits [[e1;e2;e3]] would
          yield the mergers [merge ((merge e1 e2) e3)] (we omitted the
          I/O map and partition of transforms).

            If no optimisation is required, then [edits] is returned
          instead (the default is no optimisation). *)

       val build :
         ?opt:bool ->
         IO_map.t ->
         edit list ->           (* TODO: WHY A LIST ??? *)
         IO_desc.t * edit list

       (* The evaluation of the call [apply edit desc] applies the
          edit [edit], using the I/O descriptors [desc]. The result is
          a string buffer and, in case of error, a partial string
          buffer may be returned. *)

       val apply : edit -> IO_desc.t -> (Buffer.t, Buffer.t option) result
     end

   (* Filters

      It is convenient to specify edits in a manner more abstract than
      a series of copies, writes and skips (see module type [PLAIN]
      above), in other words, by means of a high-level DSL.

        The idea is to have the default semantics be a copy of the
      input to the output, like in XSLT, and only specify the
      differences. These can be expressed with the following function
      calls and constants:

        * [insert trans ?at text filter] denotes the insertion of the
          text [text] when reaching position [at] of the input of
          transform [trans]; if [at] is absent, the insertion takes
          place immediately;

        * [overwrite trans ~at text filter] denotes the overwriting by
          the text [text] when reaching position [at] of the input of
          transform [trans];

        * [patch trans ?at ~until text filter] denotes the
          replacement, in the output of transform [trans], of the text
          between positions [at] (included) and [stop] (excluded) in
          the input of [trans], by the text [text] ([text] may be
          longer than the interval between [at] and [until]); if [at]
          is absent, the insertion takes place immediately;

        * [delete trans ?at ~until filter] denotes the deletion of the
          text between positions [at] (included) and [until]
          (excluded) in the input of transform [trans];

        * [discard trans ~until filter] denotes the skipping of the
          text from the current position in the input of transform
          [trans] until the position [pos] (excluded);

        * [copy trans ~until filter] denotes the copying of the text
          from the current position in the input of transform [trans]
          until the position [until] (excluded);

        * [skip_to trans char filter] denotes the skipping of the text
          from the current position in the input of transform [trans]
          until the character [char] (included);

        * [skip_to_end trans filter] denotes the skipping of the text
          from the current position in the input of transform [trans]
          until the end of the input;

        * [copy_to_end trans] denotes the copying of the text from the
          current position in the input of transform [trans] until the
          end of the input;

        * [append trans text filter] means copying until the end of
          the input of transform [trans] and inserting the string
          [text];

        * [stop] denotes the end of the differences between the input
          and output of transform [trans].

     Note that, for example, calls to [insert] and [overwrite] could
     be expressed solely by means of calls to [patch], but we retain
     the former functions for ease of use, as this is consistent with
     the purpose of the type [filter].

       Some starting positions of the editing instructions above are
     optional, meaning that, when missing, they apply to the current
     position: this enables a _relative mode of editing_, with respect
     of the current position in the input, instead of an absolute mode
     (at a given position in the input, where some edition takes
     place). *)

   module Filter :
     sig
       type t
       type filter = t

       val insert      : Trans.t -> ?at:Pos.t -> string                -> t -> t
       val overwrite   : Trans.t -> at:Pos.t -> string                 -> t -> t
       val patch       : Trans.t -> ?at:Pos.t -> until:Pos.t -> string -> t -> t
       val delete      : Trans.t -> ?at:Pos.t -> until:Pos.t           -> t -> t
       val discard     : Trans.t -> until:Pos.t                        -> t -> t
       val copy        : Trans.t -> until:Pos.t                        -> t -> t
       val skip_to     : Trans.t -> char                               -> t -> t
       val skip_to_end : Trans.t                                       -> t -> t
       val append      : Trans.t -> string                             -> t -> t
       val copy_to_end : t
       val stop        : t

       (* Filters are compiled to plain edits *)

       val compile : t -> Plain.t
     end
 end
