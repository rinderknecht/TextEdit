(* I/O Maps *)

(* Ordered Types *)

module type ORD =
  sig
    type t

    val compare   : t -> t -> int
    val equal     : t -> t -> bool
    val to_string : t -> string
  end

(* The connection between transforms and their input and output
   (called "handles" is implemented by _I/O maps_. *)

module type S =
  sig
    module Trans  : ORD
    module Input  : ORD
    module Output : ORD
    module TMap   : Map.S with type key = Trans.t

    (* The type [t] maps transformations to their inputs and
       outputs. We call it here an _I/O map_. *)

    type t = (Input.t * Output.t) TMap.t

    type io_map = t

    (* The value of [empty] is an I/O map initialised with empty
       bindings.*)

    val empty : t

    (* The value of [add trans (input, output) io] is a copy of the
       I/O map [io] updated with the transform [trans] from its input
       handle [input] to its output handle [output]. Transforms are
       defined on a single input and single output, and subsequent
       calls with the same transform will replace any previous
       definition. *)

    val add : Trans.t -> Input.t * Output.t -> t -> t

    (* Conversion to string buffers *)

    val to_buffer : t -> Buffer.t
  end

(* The functor *)

module Make (Trans: ORD) (Input: ORD) (Output: ORD)
       : S with module Trans  = Trans
            and module Input  = Input
            and module Output = Output
