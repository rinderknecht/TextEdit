(* I/O Descriptors *)

(* External dependencies *)

module Pos = SourceLoc.Pos
module Partition = UnionFind.Partition

(* Ordered Types (signatures of transformations) *)

module type ORD =
  sig
    type t

    val compare   : t -> t -> int
    val equal     : t -> t -> bool
    val to_string : t -> string
  end

(* I/O of characters *)

(* Input blocks

   _Input blocks_ are a source of characters, with a position
   corresponding to the last read character. That position is encoded
   by a value of type [Pos.t], as it would be in a lexing buffer of
   type [Lexing.lexbuf] (indeed, [Pos.t] is based on
   [Lexing.position]), because we want to be able to express where
   edits occur in terms of lines and columns or horizontal
   offsets. This additional position enables to resume reading from
   where we left. When no characters are left, the value returned by
   [read] is [None]. *)

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

(* Output Blocks

   Output blocks do not contain a position (see value [where] in
   signature [IN_BLOCK]), because we always append to them (so the
   position is implicitly the last character in the buffer). If the
   output block is finite and full, [write] will return the same
   block. Depending on the implementation of the signature
   [OUT_BLOCK], the function [close] may or may not be the
   identity. *)

module type OUT_BLOCK =
  sig
    type t
    module IO_map : IO_map.S

    val make  : IO_map.Output.t -> t
    val write : t -> char -> t
    val close : t -> unit
  end

(* I/O Descriptors's Signature *)

module type S =
  sig
    (* Descriptors *)

    module InBlock  : IN_BLOCK
    module OutBlock : OUT_BLOCK

    module Trans    : ORD
    module TMap     : Map.S with type key = Trans.t

    type t = (InBlock.t * OutBlock.t) TMap.t
    type desc = t

    val empty : t

    (* Making descriptors

       Because we want to share input blocks between transformations
       that have meargeable edits (making overlays), the argument
       [part] of [make part map] represents a partition of equivalent
       transformations.

         For example, let us suppose that we have three
       transformations, denoted by the constant data constructors
       [Pre], [Mid] and [Post] from the same type. Let us further
       assume that, in the I/O map, it is recorded that all apply to
       the same input block "foo". Moreover, let us suppose that the
       partition [part] (of equivalence classes) of transformations,
       tells us that [Pre] and [Mid] are equivalent, but neither is
       equivalent to [Post]. This means that [Pre] and [Mid] are
       mergeable, that is, an edit can be made that performs only one
       pass over the input block denoted by "foo" and has the same
       effects as both of them being applied sequentially (two passes)
       on two copies of the input block. On the other hand, [Post]
       must be applied separately because it conflicts with either
       [Pre] or [Mid], or both. In sum, we can do the editing in two
       passes instead of three. With this information, the input
       descriptor records that [Pre] and [Dir] are mapped to the
       _same_ input block "foo" , whereas [Post] has its own input
       block for "foo". *)

    module TEq    : Partition.S with type item = Trans.t
    module IO_map : IO_map.S

    val make      : TEq.partition -> IO_map.t -> t
    val make_desc : TEq.partition -> IO_map.t -> t (* Alias *)
  end

(* Functor *)

module Make (IO_map   : IO_map.S)
            (InBlock  : IN_BLOCK  with module IO_map = IO_map)
            (OutBlock : OUT_BLOCK with module IO_map = IO_map)
       : S with module IO_map = IO_map
            and module Trans  = IO_map.Trans
