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
   whose evaluation on input strings produces output text
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

module Pos     = SourceLoc.Pos
module PolySet = RedBlackTrees.PolySet
module PolyMap = RedBlackTrees.PolyMap

(* Transforms *)

module type TRANS =
  sig
    type t
    type trans = t

    val compare   : t -> t -> int
    val equal     : t -> t -> bool
    val to_string : t -> string
  end

(* I/O Maps *)

module IO_map :
  sig
    (* As explained above, inputs and outputs are texts. To denote
       them and use them as keys in maps, we need to refer to them by
       means of unique names, as we would do with files. Those names
       are called here _handles_. *)

    type handle = string

    (* We need now to connect transforms and handles.

       The type ['trans binding] describes a two-way mapping between
       handles and transforms, both for input and output.

         * The field [lift] maps a handle to a set of transforms (see
           module [PolySet] in library RedBlackTrees), because a
           handle can be used by multiple transforms, either as input
           or output.

         * Conversely, the field [drop] maps a transform to a handle,
           as each transform is associated to one handle, either as
           input or output.

       Technical note: We do not use the [Map] module from the
       standard library because we need the polymorphism on the
       transforms, hence the need for the module [PolySet], despite
       the lack of static guarantee on the comparison of transforms,
       and possible lack of balance of the tree in the worst case. *)

    type 'trans binding = {
      lift : (handle, 'trans PolySet.t) PolyMap.t;
      drop : ('trans, handle) PolyMap.t
    }

    (* The type ['trans t] gathers bindings transform-handle for input
       and output. We call it here an _I/O map_. *)

    type 'trans t = {
      input     : 'trans binding;
      output    : 'trans binding;
      to_string : 'trans t -> string  (* For debug *)
    }

    type 'trans io_map = 'trans t

    (* The value of [init to_string] is an I/O map initialised with
       empty bindings and the function [to_string] that provides a
       string representing a transform. *)

    val init : ('trans t -> string) -> 'trans t

    (* The value of [add (input, trans, output) io] is a copy of the
       I/O map [io] updated with the transform [trans] from its input
       handle [input] to its output handle [output]. Transforms are
       defined on a single input and single output, and subsequent
       calls with the same transform will replace any previous
       definition. *)

    val add : (handle * 'trans * handle) -> 'trans t -> 'trans t

    (* Printing of I/O maps (for debug) *)

    val print_io_map : 'trans t -> Buffer.t
  end

(* Descriptors *)

(* Handles are lowered to different data structures, depending if on
   the input or the input side. Those mappings are called _I/O
   descriptors_. *)


module IO_desc :
  sig
    (* On the input side, handles are mapped to lexing buffers (of
       type [Lexing.lexbuf) with a position corresponding to the last
       read character. This is enough to resume resume reading from
       where we left. *)

    type 'trans in_desc  = ('trans, Pos.t * Lexing.lexbuf) PolyMap.t

    (* On the output side, handles are mapped to text buffers (of type
       [Buffer.t]. Contrary to inputs, this kind of I/O descriptor
       does not contain a position in the text buffer, because we
       always append to it (so the position is implicitly the last
       character in the buffer). *)

    type 'trans out_desc = ('trans, Buffer.t) PolyMap.t

    (* The type ['trans t] gathers I/O descriptors for inputs and
       outputs. *)

    type 'trans t = {
      in_desc  : 'trans in_desc;
      out_desc : 'trans out_desc
    }

    type 'trans io_desc = 'trans t
  end

(* A Low-Level DSL for editing *)

(* The signature [PLAIN] specifies a tiny Domain Specific Language
   (DSL) by means of an abstract syntax whose interpretation results
   in editing the text of possibly multiple files, but not
   in-place. Several output text buffers may be produced as a
   result. *)

module Plain :
  sig
    (* An edit is parameterised by a set of transforms. More
       precisely, the type of an edit is ['tset t] where ['tset]
       should be a sum type whose variants denote each a
       transform. This enables an edit to combine multiple
       transforms. *)

    type 'tset t
    type 'tset edit = 'tset t

    (* Edits of type ['tset t] are denoted by the values of the
       following function calls and constants:

         * [null] is the empty edit, a no-operation;

         * the edit [write trans text edit] denotes the action of
           writing the string [text] into the output of the transform
           [trans], followed by the edit [edit] to be applied next;

         * the edit [copy trans ~until:pos edit] represents the action
           of copying the characters in the input of transform [trans]
           until the position [pos] (excluded), and writing them to
           the output of [trans], followed by the edit [edit] to be
           applied next;

         * the edit [skip trans ~until:pos edit] denotes the action of
           skipping the text in the input until the position [pos]
           (excluded) is reached, followed by the edit [edit] to be
           applied next;

         * the edit [find trans char edit] means skipping the text in
           the input until the character [char] (included) is reached,
           followed by the edit [edit] to be applied next. Note that
           this edit, contrary to the others, is not static, as it
           depends on the contents of the input text.

       Note how edits can combine multiple transforms by simply
       passing a different transform (first argument) to the functions
       above. *)

    val null  : 'tset t
    val write : 'tset -> string      -> 'tset t -> 'tset t
    val copy  : 'tset -> until:Pos.t -> 'tset t -> 'tset t
    val skip  : 'tset -> until:Pos.t -> 'tset t -> 'tset t
    val find  : 'tset -> char        -> 'tset t -> 'tset t

    (* The value of the call [check edit] is [true] if, and only if,
       the positions enabling the individual edits are strictly
       increasing for a given transformation, so they can be applied
       in one pass over the corresponding input. If this condition is
       not satisfied, and error reports the first two positions that
       violate the condition. *)

    val check : 'tset t -> (unit, Pos.t * Pos.t) Stdlib.result

    (* The call [show ~offsets io edits] prints the edits [edits]
       interpreted with respect to the I/O map [io]. *)

    val show :
      offsets:bool -> 'trans IO_map.t -> 'tset list -> Buffer.t

    (* Making the I/O descriptors and optionally optimising lists of
       edits by merging them pairwise whenever possible, like a
       peep-hole optimisation. Indeed, edits on the same input can be
       defined separately and this module implements a function which
       tries to merge pairwise edits that are sequentially composable,
       in order to minimise the number of passes on a given input
       source.

         The value of [build ~opt io edits] is a pair whose first
       component is an I/O descriptor, of type ['trans desc]. If the
       call is [build ~opt:true io edits], then the second component
       is a list of edits equivalent to the list [edits], but
       hopefully shorter due to possible mergers between contiguous
       edits within the list. Mergers are left-associative, so the
       list of edits [[e1;e2;e3]] would yield the mergers [merge
       ((merge e1 e2) e3)] (we omitted the I/O map and partition of
       transforms).

         If no optimisation is required, then [edits] is returned
       instead (the default is no optimisation). *)

    val build :
      ?opt:bool ->
      'trans IO_map.t ->
      'tset edit list ->
      'trans IO_desc.t * 'tset edit list

    (* The evaluation of the call [apply edits desc] applies the edits
       in [edits], using the I/O descriptors [desc] to interpret the
       lexing buffers as inputs and the text buffers as outputs. The
       result is a text buffer and, in case of error, a partial text
       buffer may be returned. *)

    val apply :
      'tset edit ->
      'trans IO_desc.t ->
      (Buffer.t, Buffer.t option) result
  end

(* Filters

   It is convenient to specify edits in a manner more abstract than a
   series of copies, writes and skips (see module type [PLAIN] above),
   in other words, by means of a high-level DSL.

     The idea is to have the default semantics be a copy of the input
   to the output, like in XSLT, and only specify the
   differences. These can be expressed with the following function
   calls and constants:

     * [insert trans ?at text filter] denotes the insertion of the
       text [text] when reaching position [at] of the input of
       transform [trans]; if [at] is absent, the insertion takes
       place immediately;

     * [overwrite trans ~at text filter] denotes the overwriting by the
       text [text] when reaching position [at] of the input of
       transform [trans];

     * [patch trans ?at ~until text filter] denotes the replacement,
       in the output of transform [trans], of the text between
       positions [at] (included) and [stop] (excluded) in the input of
       [trans], by the text [text] ([text] may be longer than the
       interval between [at] and [until]); if [at] is absent, the
       insertion takes place immediately;

     * [delete trans ?at ~until filter] denotes the deletion of the
       text between positions [at] (included) and [until] (excluded)
       in the input of transform [trans];

     * [discard trans ~until filter] denotes the skipping of the text
       from the current position in the input of transform [trans]
       until the position [pos] (excluded);

     * [copy trans ~until filter] denotes the copying of the text from
       the current position in the input of transform [trans] until
       the position [until] (excluded);

     * [skip_to trans char filter] denotes the skipping of the text
       from the current position in the input of transform [trans]
       until the character [char] (included);

     * [skip_to_end trans filter] denotes the skipping of the text
       from the current position in the input of transform [trans]
       until the end of the input;

     * [copy_to_end trans] denotes the copying of the text from the
       current position in the input of transform [trans] until the
       end of the input;

     * [append trans text filter] means copying until the end of the
       input of transform [trans] and inserting the string [text];

     * [stop] denotes the end of the differences between the input and
       output of transform [trans].

  Note that, for example, calls to [insert] and [overwrite] could be
  expressed solely by means of calls to [patch], but we retain the
  former functions for ease of use, as this is consistent with the
  purpose of the type [filter].

    Some starting positions of the editing instructions above are
  optional, meaning that, when missing, they apply to the current
  position: this enables a _relative mode of editing_, with respect of
  the current position in the input, instead of an absolute mode (at a
  given position in the input, where some edition takes place). *)

module Filter :
  sig
    type 'tset t
    type 'tset filter = 'tset t

    val insert      : 'tset -> ?at:Pos.t -> string                -> 'tset t -> 'tset t
    val overwrite   : 'tset -> at:Pos.t -> string                 -> 'tset t -> 'tset t
    val patch       : 'tset -> ?at:Pos.t -> until:Pos.t -> string -> 'tset t -> 'tset t
    val delete      : 'tset -> ?at:Pos.t -> until:Pos.t           -> 'tset t -> 'tset t
    val discard     : 'tset -> until:Pos.t                        -> 'tset t -> 'tset t
    val copy        : 'tset -> until:Pos.t                        -> 'tset t -> 'tset t
    val skip_to     : 'tset -> char                               -> 'tset t -> 'tset t
    val skip_to_end : 'tset                                       -> 'tset t -> 'tset t
    val append      : 'tset -> string                             -> 'tset t -> 'tset t
    val copy_to_end : 'tset t
    val stop        : 'tset t

    (* Filters are compiled to plain edits *)

    val compile : t -> Plain.t
  end
