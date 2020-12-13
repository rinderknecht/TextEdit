(* I/O Maps *)

(* Ordered Types *)

module type ORD =
  sig
    type t

    val compare   : t -> t -> int
    val equal     : t -> t -> bool
    val to_string : t -> string
  end

(* Signature of the functor *)

module type S =
  sig
    module Trans  : ORD
    module Input  : ORD
    module Output : ORD
    module TMap   : Map.S with type key = Trans.t

    type t = (Input.t * Output.t) TMap.t
    type io_map = t

    val empty : t

    val add : Trans.t -> Input.t * Output.t -> t -> t

    val to_buffer : t -> Buffer.t
  end

(* The functor *)

module Make (Trans: ORD) (Input: ORD) (Output: ORD) =
  struct
    module Trans  = Trans
    module Input  = Input
    module Output = Output
    module TMap   = Map.Make (Trans)

    type t = (Input.t * Output.t) TMap.t
    type io_map = t

    let empty = TMap.empty
    let add   = TMap.add

    let to_buffer io =
      let buffer = Buffer.create 131 in
      let print trans (input, output) =
        let string =
          Printf.sprintf "%s -> %s, %s\n"
                         (Trans.to_string trans)
                         (Input.to_string input)
                         (Output.to_string output)
        in Buffer.add_string buffer string
      in TMap.iter print io; buffer
  end
