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

(* Signature *)

module type S =
  sig
    module Input  : ORD
    module Output : ORD

    module InMap  : Map.S with type key = Input.t
    module OutMap : Map.S with type key = Output.t

    type in_desc  = (Buffer.t * Pos.t) InMap.t
    type out_desc = Buffer.t OutMap.t
    type t        = {in_desc : in_desc; out_desc : out_desc}
    type io_desc  = t

    val empty : t
  end

(* Functor *)

module Make (Input: ORD) (Output: ORD) =
  struct
    module Input = Input
    module Output = Output

    module InMap  = Map.Make (Input)
    module OutMap = Map.Make (Output)

    type in_desc  = (Buffer.t * Pos.t) InMap.t
    type out_desc = Buffer.t OutMap.t
    type t        = {in_desc : in_desc; out_desc : out_desc}
    type io_desc  = t

    let empty = {
      in_desc  = InMap.empty;
      out_desc = OutMap.empty
    }
  end
