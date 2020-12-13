(* I/O Descriptors *)

(* External dependencies *)

module Pos = SourceLoc.Pos
module Partition = UnionFind.Partition

(* Ordered Types *)

module type ORD =
  sig
    type t

    val compare   : t -> t -> int
    val equal     : t -> t -> bool
    val to_string : t -> string
  end

(* I/O of characters *)

module type IN_BLOCK =
  sig
    type t
    module IO_map : IO_map.S

    val make  : IO_map.Input.t -> t
    val read  : t -> char option
    val where : t -> Pos.t
    val copy  : t -> t
    val close : t -> unit
  end

module type OUT_BLOCK =
  sig
    type t
    module IO_map : IO_map.S

    val make  : IO_map.Output.t -> t
    val write : t -> char -> t
    val close : t -> unit
  end

(* Signature *)

module type S =
  sig
    module InBlock  : IN_BLOCK
    module OutBlock : OUT_BLOCK

    module Trans    : ORD
    module TMap     : Map.S with type key = Trans.t

    type t = (InBlock.t * OutBlock.t) TMap.t
    type desc = t

    val empty : t

    module TEq    : Partition.S with type item = Trans.t
    module IO_map : IO_map.S

    val make      : TEq.partition -> IO_map.t -> t
    val make_desc : TEq.partition -> IO_map.t -> t (* Alias *)
  end

(* Functor *)

module Make (IO_map   : IO_map.S)
            (InBlock  : IN_BLOCK  with module IO_map = IO_map)
            (OutBlock : OUT_BLOCK with module IO_map = IO_map) =
  struct
    module InBlock  = InBlock
    module OutBlock = OutBlock
    module Trans    = IO_map.Trans
    module TMap     = IO_map.TMap

    type t = (InBlock.t * OutBlock.t) TMap.t
    type desc = t

    let empty = TMap.empty

    (* To compute the input descriptors from equivalent classes of
       transformations and an I/O map, we iterate over the list of
       input handles and apply to them the function [delta]. For each
       pair of transformation [trans] and handle [handle], we fetch
       the representative [repr] of [trans] in [part]. If [repr] is
       already mapped (in the accumulator [acc] of the fold), then we
       record the transformation as sharing the same
       descriptor. Otherwise, we create a fresh descriptor and record
       the representative [repr] of [trans] in the accumulator. If the
       representative is the same transformation as [trans], we are
       done, otherwise we also record [trans].

         Let us consider an example. We have five transformations:
       [Pre], [Ori], [Dir], [Post] and [Inc]. The I/O map states that
       all share the same input buffer denoted by the handle "foo",
       and [Pre], [Ori], [Dir] and [Post] share the same output buffer
       (denoted by the handle) "bar", whereas [Inc] has the output
       "baz". (The reverse map says that "foo" is read by [Pre],
       [Ori], [Dir] and [Post] etc.)  There are two equivalence
       classes: {[Pre], [Ori], [Dir], [Post]} and {[Incl]}, so only
       two passes over the buffer "foo" are sufficient, instead of
       five. The function [mk_in_desc] takes these two pieces of
       information and maps all the transformations in the first class
       to a shared input descriptor, and [Incl] to a different
       descriptor. Here is how.

         The equivalence classes can be drawn as a forest with upward
       edges and an implicit loop on the roots, which are the
       representative of the class modelled by the tree. (In other
       words, they are their own representative.) Let us assume that
       we have the following forest:

                 Ori         Inc
                / | \
             Pre Dir Post

         (The assumption bears on [Ori] being the representative:
       another transformation in the same class would do as well.)
       First, the map is empty. Let us assume that a binding in
       [io.input.drop] links [Pre] to the buffer named "foo". The
       representative of [Pre] is [Ori], which is not in the
       accumulator, hence a new descriptor is created and [Ori] is
       mapped to it. Since [Pre] is different from [Ori], we also need
       to map [Pre] to the same descriptor. Then we encounter the
       binding of [Dir] to the buffer "foo". The representative of
       [Dir] is [Ori], which we now find in the accumulator. We then
       simply map [Dir] to the descriptor of [Ori]. Next, let us
       suppose that we visit the binding of [Post] to the buffer
       "foo". The representative of [Post] is [Ori], which is already
       recorded, so we map [Post] to the descriptor of [Ori]. Finally,
       we find the binding of [Inc] to "foo". It is not found in the
       accumulator, hence it is mapped to a fresh descriptor. Because
       [Inc] is its own representative, we are finished. *)

    module TEq = UnionFind.Partition2.Make (Trans)
    module IO_map = IO_map

    let make (partition : TEq.partition) (io : IO_map.t) =
      let module OutMap = Map.Make (IO_map.Output) in
      let delta trans (input, output) (out_map, desc) =
        let out_map, out_block =
          match OutMap.find_opt output out_map with
            Some out_block ->
              out_map, out_block
          | None ->
              let out_block = OutBlock.make output in
              let out_map   = OutMap.add output out_block out_map
              in out_map, out_block
        and repr = TEq.repr trans partition in
        match TMap.find_opt repr desc with
          Some (in_block, _) ->
            out_map, TMap.add trans (in_block, out_block) desc
        | None ->
            let blocks = InBlock.make input, out_block in
            let desc   = TMap.add repr blocks desc in
            let desc   = if Trans.equal repr trans then desc
                         else TMap.add trans blocks desc
            in out_map, desc
      in TMap.fold delta io (OutMap.empty, TMap.empty) |> snd

    let make_desc = make
  end
