(* I/O Descriptors *)

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

(* Handles are lowered to different data structures, depending if on
   the input or the input side. Those mappings are called _I/O
   descriptors_. *)

module type S =
  sig
    module Input  : ORD
    module Output : ORD

    module InMap  : Map.S with type key = Input.t
    module OutMap : Map.S with type key = Output.t

    (* Input handles are mapped to string buffers of type [Buffer.t],
       coupled with a position in them corresponding to the last read
       character. Note that the position is encoded by a value of type
       [Pos.t], as it would be in a lexing buffer of type
       [Lexing.lexbuf] (indeed, [Pos.t] is based on
       [Lexing.position]), because we want to be able to express where
       edits occur in terms of lines and columns or horizontal
       offsets. This additional position enables to resume reading
       from where we left. *)

    type in_desc  = (Buffer.t * Pos.t) InMap.t

    (* Contrary to input descriptors, output descriptor do not contain
       a position in the string buffer, because we always append to it
       (so the position is implicitly the last character in the
       buffer). *)

    type out_desc = Buffer.t OutMap.t

    (* The type [t] gathers I/O descriptors for inputs and
       outputs. *)

    type t = {in_desc : in_desc; out_desc : out_desc}

    type io_desc = t

    (* Empty descriptor *)

    val empty : t
  end

(* Functor *)

module Make (Input: ORD) (Output: ORD)
       : S with module Input  = Input
            and module Output = Output
