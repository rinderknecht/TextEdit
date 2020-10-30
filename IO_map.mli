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

    module InMap  : Map.S with type key = Input.t
    module OutMap : Map.S with type key = Output.t
    module TSet   : Set.S with type elt = Trans.t
    module TMap   : Map.S with type key = Trans.t

    (* The types [in_binding] and [out_binding] describe a two-way
       mapping between handles and transforms, both for input and
       output.

         * The field [lift] maps a handle to a set of transforms
           because a handle can be used by multiple transforms,
           either as input or output.

         * Conversely, the field [drop] maps a transform to a
           handle, as each transform is associated to one input
           handle and one output handle. *)

    type in_binding = {
      lift : TSet.t InMap.t;
      drop : Input.t TMap.t
    }

    type out_binding = {
      lift : TSet.t OutMap.t;
      drop : Output.t TMap.t
    }

    (* The type [t] gathers bindings for input and output. We call
       it here an _I/O map_. *)

    type t = {
      input  : in_binding;
      output : out_binding
    }

    type io_map = t

    (* The value of [empty] is an I/O map initialised with empty
       bindings.*)

    val empty : t

    (* The value of [add (input, trans, output) io] is a copy of the
       I/O map [io] updated with the transform [trans] from its input
       handle [input] to its output handle [output]. Transforms are
       defined on a single input and single output, and subsequent
       calls with the same transform will replace any previous
       definition. Note that we do not distinghush between input and
       output handles, in other words, they have the same type, and
       this means that an input can be extended (only) by its own
       editing. *)

    val add : Input.t * Trans.t * Output.t -> t -> t

    (* Printing of I/O maps (for debug) *)

    val to_string : t -> string
  end

(* The functor *)

module Make (Trans: ORD) (Input: ORD) (Output: ORD)
       : S with module Trans  = Trans
            and module Input  = Input
            and module Output = Output
