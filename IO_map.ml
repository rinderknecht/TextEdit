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

(* Signature of the functor *)

module type S =
  sig
    module Trans  : ORD
    module Input  : ORD
    module Output : ORD

    module InMap  : Map.S with type key = Input.t
    module OutMap : Map.S with type key = Output.t
    module TSet   : Set.S with type elt = Trans.t
    module TMap   : Map.S with type key = Trans.t

     type in_binding = {
       lift : TSet.t InMap.t;
       drop : Input.t TMap.t
     }

     type out_binding = {
       lift : TSet.t OutMap.t;
       drop : Output.t TMap.t
     }

     type t = {
       input  : in_binding;
       output : out_binding
     }

     type io_map = t

     val empty : t

     val add : Input.t * Trans.t * Output.t -> t -> t

     val to_string : t -> string
  end

(* The functor *)

module Make (Trans: ORD) (Input: ORD) (Output: ORD) =
  struct
    module Trans  = Trans
    module Input  = Input
    module Output = Output

    module InMap  = Map.Make (Input)
    module OutMap = Map.Make (Output)
    module TSet   = Set.Make (Trans)
    module TMap   = Map.Make (Trans)

     type in_binding = {
       lift : TSet.t InMap.t;
       drop : Input.t TMap.t
     }

     type out_binding = {
       lift : TSet.t OutMap.t;
       drop : Output.t TMap.t
     }

     type t = {
       input  : in_binding;
       output : out_binding
     }

     type io_map = t

     let empty : t = {
       input = {
         lift = InMap.empty;
         drop = TMap.empty
       };
       output = {
         lift = OutMap.empty;
         drop = TMap.empty
       }
     }

     let add_out trans output io : t =
       let t_set = try OutMap.find output io.output.lift with
                     Not_found -> TSet.empty in
       let lift  = OutMap.add output (TSet.add trans t_set) io.output.lift
       and drop  = TMap.add trans output io.output.drop
       in {io with output = {lift; drop}}

     let add_in trans input io : t =
       let t_set = try InMap.find input io.input.lift with
                     Not_found -> TSet.empty in
       let lift  = InMap.add input (TSet.add trans t_set) io.input.lift
       and drop  = TMap.add trans input io.input.drop
       in {io with input = {lift; drop}}

     let add (input, trans, output) io =
         add_out trans output @@ add_in trans input io
        (* Pretty-printing of I/O maps *)

        let print_in_drop buffer trans handle =
          let trans  = Trans.to_string trans
          and handle = Input.to_string handle in
          Buffer.add_string buffer
          @@ Printf.sprintf "%s: %s\n" trans handle

        let print_in_lift buffer handle tset =
          let tset   = TSet.elements tset in
          let tset   = List.map Trans.to_string tset in
          let tset   = String.concat ", " tset
          and handle = Input.to_string handle in
          let entry  = Printf.sprintf "     %s -> {%s}\n" handle tset
          in Buffer.add_string buffer entry

        let print_in_bindings buffer (binding: in_binding) =
          Buffer.add_string buffer " * Lift:\n";
          InMap.iter (print_in_lift buffer) binding.lift;
          Buffer.add_string buffer " * Drop:\n";
          TMap.iter (print_in_drop buffer) binding.drop

        let print_out_drop buffer trans handle =
          let trans  = Trans.to_string trans
          and handle = Output.to_string handle in
          Buffer.add_string buffer
          @@ Printf.sprintf "%s: %s\n" trans handle

        let print_out_lift buffer handle tset =
          let tset   = TSet.elements tset in
          let tset   = List.map Trans.to_string tset in
          let tset   = String.concat ", " tset
          and handle = Output.to_string handle in
          let entry  = Printf.sprintf "     %s -> {%s}\n" handle tset
          in Buffer.add_string buffer entry

        let print_out_bindings buffer (binding: out_binding) =
          Buffer.add_string buffer " * Lift:\n";
          OutMap.iter (print_out_lift buffer) binding.lift;
          Buffer.add_string buffer " * Drop:\n";
          TMap.iter (print_out_drop buffer) binding.drop

        let to_string io =
          let buffer = Buffer.create 131 in
          Buffer.add_string buffer "Input:\n";
          print_in_bindings buffer io.input;
          Buffer.add_string buffer "Output:\n";
          print_out_bindings buffer io.output;
          Buffer.contents buffer
  end
