(* A Low-Level DSL for editing *)

(* External dependencies *)

module Pos = SourceLoc.Pos

(* Ordered Types *)

module type ORD =
  sig
    type t

    val compare   : t -> t -> int
    val equal     : t -> t -> bool
    val to_string : t -> string
  end

(* The module [Plain] specifies a tiny Domain Specific Language (DSL)
   by means of an abstract syntax whose interpretation results in
   editing possibly multiple buffers, but not in-place. Several output
   buffers may be produced as a result. *)

module type S =
  sig
    (* An edit is parameterised by a set of transforms. For example,
       the type of an edit could be a sum type whose variants denote
       each a transform. This parameterisation enables an edit to
       combine multiple transforms. *)

    module IO_map : IO_map.S
    module Trans = IO_map.Trans

    type t
    type edit = t

    (* Edits of type [t] are denoted by the values of the following
       function calls and constants:

         * [null] is the empty edit, a no-operation;

         * the edit [write trans text edit] denotes the action of
           writing the string [text] into the output of the transform
           [trans], followed by the edit [edit] to be evaluated next;

         * the edit [copy trans ~until:pos edit] represents the action
           of copying the characters in the input of transform [trans]
           until the position [pos] (excluded), and writing them to
           the output of [trans], followed by the edit [edit] to be
           evaluated next;

         * the edit [skip trans ~until:pos edit] denotes the action of
           skipping the text in the input until the position [pos]
           (excluded) is reached, followed by the edit [edit] to be
           evaluated next;

         * the edit [goto trans char edit] means skipping the text in
           the input until the character [char] (included) is reached,
           followed by the edit [edit] to be evaluated next. Note that
           this edit, contrary to the others, is not static, as it
           depends on the contents of the input text.

       Note how edits can combine multiple transforms by simply
       passing a different transform (first argument) to the
       functions above. *)

    val null  : t
    val write : Trans.t -> string      -> t -> t
    val copy  : Trans.t -> until:Pos.t -> t -> t
    val skip  : Trans.t -> until:Pos.t -> t -> t
    val goto  : Trans.t -> char        -> t -> t

    (* Edits as strings *)

    val to_buffer :
      offsets:bool -> IO_map.t -> t -> (Buffer.t, Buffer.t) Stdlib.result

    (* The value of the call [check ~offsets io edit] is [()] if, and
       only if, the positions enabling the individual edits are
       strictly increasing for a given transformation, so they can be
       applied in one pass over the corresponding input. If this
       condition is not satisfied, an error message reports the first
       two edits that violate the condition. *)

    val check :
      offsets:bool -> IO_map.t -> t -> (unit, string) Stdlib.result

    (* The value of [reduce io edit] is an edit whose operational
       semantics is the same as that of [edit] (that is, its
       application to the same input yields the same output), but is
       possibly shorter due to a peep-hole optimisation. *)

    val reduce :
      offsets:bool -> IO_map.t -> t -> (t, string) Stdlib.result

    (* TEMPORARY *)

    module Input = IO_map.Input
    module InMap : Map.S with type key = Input.t
    module TSet  : Set.S with type elt = Trans.t

    val extract_edits : IO_map.t -> t -> (t * TSet.t) InMap.t

    (* Optimising maps according to mergeable transformations *)

    module TEq : Partition.S with type item = Trans.t

   (*    val optimise : t -> TEq.partition * t*)
(*
    (* The call [show ~offsets io edit] prints the edit [edit]
       interpreted with respect to the I/O map [io]. *)

    module IO_map  : IO_map.S

    val show : offsets:bool -> IO_map.t -> edit -> Buffer.t

    (* Making the I/O descriptors and optionally optimising lists of
       edits by merging them pairwise whenever possible, like a
       peep-hole optimisation. Indeed, edits on the same input can be
       defined separately and this module implements a function which
       tries to merge pairwise edits that are sequentially composable,
       in order to minimise the number of passes on a given input
       source.

          TODO: WHY A LIST ???

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

    module IO_desc : IO_desc.S

    val build :
      ?opt:bool ->
      IO_map.t ->
      edit ->
      IO_desc.t * edit

    (* The evaluation of the call [eval edit desc] evaluates the
       edit [edit], using the I/O descriptors [desc]. The result is
       a string buffer and, in case of error, a partial string
       buffer may be returned. *)

 (* val apply : edit -> IO_desc.t -> (Buffer.t, Buffer.t option) result *)

    type env = <map : IO_map.t; desc : IO_desc.t>

    val eval : env -> edit -> unit
 *)
  end

module Make (IO_map : IO_map.S) : S with module IO_map = IO_map
