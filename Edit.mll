(* Editing text files functionally *)

{
(* HEADER *)

(* External dependencies *)

module Pos       = SourceLoc.Pos
module Partition = UnionFind.Partition2

(* Utilities *)

let (<@) f g x = f (g x)

(* Transforms *)

module type TRANS =
  sig
    type t
    type trans = t

    val compare   : t -> t -> int
    val equal     : t -> t -> bool
    val to_string : t -> string
  end

module Make (Trans : TRANS) =
  struct
    type handle = string

    module HMap : Map.S with type key = handle
    module TSet : Set.S with type elt = Trans.t
    module TMap : Map.S with type key = Trans.t
    module TEq  : Partition.S with type item = Trans.t

    (* I/O Maps *)

    module IO_map =
      struct
        type binding = {
          lift : TSet.t HMap.t;
          drop : handle TMap.t
        }

        type t = {
          input     : binding;
          output    : binding;
          to_string : t -> string  (* For debug *)
        }

        type io_map = t

        let init mk_str =
          let empty = {lift = HMap.empty; drop = TMap.empty}
          in {input = empty; output = empty; to_string = mk_str}

        (* Adding handlers *)

        let add_in trans handle io =
          let t_set = try HMap.find handle io.input.lift with
                        Not_found -> TSet.empty in
          let lift  = HMap.add handle (TSet.add trans t_set) io.input.lift
          and drop  = TMap.add trans handle io.input.drop
          in {io with input = {lift; drop}}

        let add_out trans handle io =
          let t_set = try HMap.find handle io.output.lift with
                        Not_found -> TSet.empty in
          let lift  = HMap.add file (TSet.add trans t_set) io.output.lift
          and drop  = TMap.add trans file io.output.drop
          in {io with output = {lift; drop}}

        type input  = handle
        type output = handle

        let add (input, trans, output) io =
          add_out trans output @@ add_in trans input io
        (* Pretty-printing of I/O maps (of type [io_map]) *)

        let print_trans buffer to_string trans handle =
          let s = Printf.sprintf "%s: %s\n" (to_string trans) handle
          in Buffer.add_string buffer s

        let print_handle buffer to_string handle tree =
          let show_trans tree = Buffer.add_string (to_string tree ^ ", ")
          in Buffer.add_string buffer (Printf.sprintf "%s -> {" handle);
             TSet.iter (show_trans buffer) tree;
             Buffer.add_string buffer "}"

        let print_bindings buffer to_string {lift; drop} =
          Buffer.add_string buffer " * Lift:\n";
          HMap.iter (print_handle buffer to_string) lift;
          Buffer.add_string buffer " * Drop:\n";
          TMap.iter (print_trans buffer to_string) drop

        let print io =
          let buffer = Buffer.create 131 in
          Buffer.add_string buffer "Displaying input:";
          print_bindings buffer io.to_string io.input;
          Buffer.add_string buffer "Displaying output:";
          print_bindings buffer io.to_string io.output;
          buffer
      end

    (* I/O Descriptors *)

    module IO_desc =
      struct
        (* Mapping edits to their descriptors *)

        type in_desc  = (Pos.t * Buffer.t) TMap.t
        type out_desc = Buffer.t TMap.t
        type desc     = {in_desc : in_desc; out_desc : out_desc}

        (* From I/O maps to I/O descriptors

             When making input descriptors from input maps, handles
           are interpreted as buffer names, and the value of
           [mk_in_desc io part] is an input descriptor, that is, a map
           from transformations to a string buffer and file position
           (the first position). Because we want to share input
           buffers between transformations that have meargeable edits
           (we could think of them as non-overlapping overlays), the
           argument [part] represents a partition of equivalent
           transformations.

             For example, let us suppose that we have three
           transformations, distinguished as constant data
           constructors [Pre], [Mid] and [Post] from the same
           type. Let us further assume that, in the I/O map, it is
           recorded that all apply to the same input buffer denoted by
           the handle "foo". Moreover, let us suppose that the
           partition [part] (of equivalence classes) of
           transformations, tells us that [Pre] and [Mid] are
           equivalent, but not equivalent to [Post]. This means that
           [Pre] and [Mid] are mergeable, that is, an edit can be made
           that performs only one pass over the input buffer denoted
           by "foo" and has the same effects as both of them being
           applied sequentially (two passes). On the other hand,
           [Post] must be applied separately because it conflicts with
           either [Pre] or [Mid] -- or both. In total, we can do the
           editing in two passes instead of three. With this
           information, the input descriptor records that [Pre] and
           [Dir] are mapped to the _same_ string buffer and position
           associated with the handle "foo" -- this mapping is called
           an I/O _descriptor_ --, whereas [Post] has is own
           descriptor for "foo".

             To compute the input descriptors, we fold the list of
           input handles and apply to them the function [delta]. For
           each pair of transformation [trans] and handle [handle], we
           fetch the representative [repr] of [trans] in [part]. If
           [repr] is already mapped (in the accumulator [acc] of the
           fold), then we record the transformation as sharing the
           same descriptor. Otherwise, we create a fresh descriptor
           and record the representative [repr] of [trans] in the
           accumulator. If the representative is the same
           transformation as [trans], we are done, otherwise we also
           record [trans].

             Consider the following example. We have five
           transformations: [Pre], [Ori], [Dir], [Post] and [Inc]. The
           I/O map states that all share the same input buffer denoted
           by the handle "foo", and [Pre], [Ori], [Dir] and [Post]
           share the same output buffer (denoted by the handle) "bar",
           whereas [Inc] has the output "baz". (The reverse map says
           that "foo" is read by [Pre], [Ori], [Dir] and [Post] etc.)
           There are two equivalence classes: {[Pre], [Ori], [Dir],
           [Post]} and {[Incl]}, so only two passes over the buffer
           "foo" are sufficient, instead of five. The function
           [mk_in_desc] takes these two pieces of information and maps
           all the transformations in the first class to a shared
           input descriptor, and [Incl] to a different
           descriptor. Here is how.

             The equivalence classes can be drawn as a forest with
           upward edges and an implicit loop on the roots, which are
           the representative of the class modelled by the tree. (In
           other words, they are their own representative.) Let us
           assume that we have the following forest:

                 Ori         Inc
                / | \
             Pre Dir Post

           (The assumption bears on [Ori] being the representative:
           another transformation in the same class would do as well.)
           First, the map is empty. Let us assume that a binding in
           [io.input.drop] links [Pre] to the buffer named "foo". The
           representative of [Pre] is [Ori], which is not in the
           accumulator, hence a new descriptor is created and [Ori] is
           mapped to it. Since [Pre] is different from [Ori], we also
           need to map [Pre] to the same descriptor. Then we encounter
           the binding of [Dir] to the buffer "foo". The
           representative of [Dir] is [Ori], which we now find in the
           accumulator. We then simply map [Dir] to the descriptor of
           [Ori]. Next, let us suppose that we visit the binding of
           [Post] to the buffer "foo". The representative of [Post] is
           [Ori], which is already recorded, so we map [Post] to the
           descriptor of [Ori]. Finally, we find the binding of [Inc]
           to "foo". It is not found in the accumulator, hence it is
           mapped to a fresh descriptor. Because [Inc] is its own
           representative, we are finished. *)

        let mk_in_desc (io: io_map) (part: TEq.partition) : in_desc =
          let delta trans handle acc =
            let repr = TEq.repr trans part in
            try TMap.add trans (TMap.find repr acc) acc with
              Not_found ->
                let buffer  = Buffer.create 131 in
                let in_desc = Pos.min ~file, buffer in
                let acc     = TMap.add repr in_desc acc in
                if Trans.equal repr trans then acc
                else TMap.add trans desc acc
          in TMap.fold delta io.input.drop TMap.empty

        (* The value of [mk_out_desc io] is an output descriptor, that
           is, a mapping of transformations to output string
           buffers. First, each handle is mapped to a fresh buffer, in
           [out_file_map].  Second, for each transformation, we fetch
           the output buffer for the associated handle, and map the
           transformation directly to that text buffer (we compose the
           two maps). *)

        let mk_out_desc (io: io_map) : out_desc =
          let apply _ handle     = HMap.add handle (Buffer.create 131) in
          let buffer_map         = TMap.fold apply io.output.drop HMap.empty in
          let delta trans handle = TMap.add trans (HMap.find handle buffer_map)
          in TMap.fold delta io.output.drop TMap.empty

    (* The call [mk_desc io part] calls [mk_in_desc] and [mk_out_desc]
       to create a complete descriptor according the I/O map [io] and
       the partition of transformations [part]. *)

    let mk_desc (io: io_map) (part: TEq.partition) : desc =
      {in_desc = mk_in_desc io part; out_desc = mk_out_desc io}

    (* Making the descriptors and optionally optimising lists of edits
       by merging them pairwise whenever possible.

       The value of [build io edits] is a pair whose first component
       is a collection of I/O descriptors, of type [desc], mapping
       transformations of type [trans] to input and output descriptors
       (respectively, lexing buffers and output channels).

       If the call is [build ~opt:true io edits], then the second
       component is a list of edits equivalent to the list [edits],
       but hopefully shorter due to possible mergers between
       contiguous edits within the list. Mergers are left-associative,
       so the list of edits [[e1;e2;e3]] would yield the mergers
       [merge ((merge e1 e2) e3)] (we omitted the I/O map and
       partition of transformations). If no optimisation is required
       by having the labelled argument [~opt:false], then [edits] is
       returned instead (the default is no optimisation).

       The optimised list of edits is computed as follows. We attempt to
       merge pairwise the edits in [edits] (see function [merge_seq]). The
       resulting edits [edits'] are then individually reduced (see
       function [reduce]) and if any final edit is trivial, it is
       discarded.

       The partition of transformations [part], which resulted from
       merging the edits, is used to create the descriptors for the
       transformations (see function [mk_desc]).

       Note that the initial partition (of equivalence classes) maps
       each transformation [trans] to itself, with height [0] (since it is
       a tree with a single node). *)

    let build ?(opt=false) (io: io_map) (edits: edit list) =
      let init =
        let apply trans _ acc = Partition.equiv trans trans acc
        in TransMap.fold apply io.input.drop Partition.empty in
      let apply edit acc =
        match reduce io edit with
          Null -> acc
        |    e -> e::acc in
      if opt then
        let edits', part = merge_seq init io edits in
        mk_desc io part, List.fold_right apply edits' []
      else mk_desc io init, edits


    (* ----------------------------------------- *)


    module Make (Trans : TRANS) =
  struct

    type trans = Trans.t

    module TransSet = Set.Make (Trans)
    module TransMap = Map.Make (Trans)
    module FileMap  = Map.Make (String)

    (* A low-level edition DSL.

       Edits are read and write operations on (possibly) multiple text
       files. For each file, edits are increasingly sorted by the
       positions where they apply. *)

    type t =
      Null
    | Copy  of trans *  Pos.t * t
    | Skip  of trans *  Pos.t * t
    | Write of trans * string * t
    | Goto  of trans *   char * t

    type edit = t

    let null                  = Null
    let copy  trans pos  edit = Copy  (trans, pos,  edit)
    let skip  trans pos  edit = Skip  (trans, pos,  edit)
    let write trans text edit = Write (trans, text, edit)
    let goto  trans char edit = Goto  (trans, char, edit)

    exception Invalid of Pos.t * Pos.t

    let check edit =
      let rec chk map = function
        Null -> () | Write (_, _, edit) | Goto (_, _, edit) ->
          chk map edit
      | Copy (trans, pos, edit) | Skip (trans, pos, edit) ->
          try
            if Pos.leq (TransMap.find trans map) pos
            then chk (TransMap.add trans pos map) edit
            else raise (Invalid (TransMap.find trans map, pos))
          with Not_found -> chk (TransMap.add trans pos map) edit
      in chk TransMap.empty edit

    (* Filters: A high-level edition DSL *)

    type filter =
      Insert    of trans * Pos.t option * string          * filter
    | Overwrite of trans * Pos.t        * string          * filter
    | Patch     of trans * Pos.t option * Pos.t  * string * filter
    | Delete    of trans * Pos.t option * Pos.t           * filter
    | Discard   of trans *                Pos.t           * filter
    | Copy    of trans *                Pos.t           * filter
    | SkipTo    of trans *                char            * filter
    | SkipToEnd of trans                                  * filter
    | Append    of trans                         * string * filter
    | CopyToEnd of trans
    | Stop

    let insert trans pos text filter       = Insert (trans, pos, text, filter)
    and overwrite trans pos text filter    = Overwrite (trans, pos, text, filter)
    and patch trans start stop text filter = Patch (trans, start, stop, text, filter)
    and delete trans start stop filter     = Delete (trans, start, stop, filter)
    and discard trans pos filter           = Discard (trans, pos, filter)
    and copy trans pos filter           = Copy (trans, pos, filter)
    and skip_to trans char filter          = SkipTo (trans, char, filter)
    and skip_to_end trans filter           = SkipToEnd (trans, filter)
    and append trans text filter           = Append (trans, text, filter)
    and copy_to_end trans                  = CopyToEnd trans
    and stop                               = Stop

    (* The function [compile] translates these changes to the
       lower-level editing DSL defined above (see type [t]). *)

    let rec compile = function
      Insert (trans, Some pos, text, filter) ->
        copy  trans pos
      @@ write trans text
      @@ compile filter
    | Insert (trans, None, text, filter) ->
        write trans text (compile filter)
    | Overwrite (trans, pos, text, filter) ->
        copy  trans pos
      @@ skip  trans (pos#shift_bytes (String.length text))
      @@ write trans text
      @@ compile filter
    | Patch (trans, Some start, stop, patch, filter) ->
        copy  trans start
      @@ skip  trans stop
      @@ write trans patch
      @@ compile filter
    | Patch (trans, None, stop, patch, filter) ->
        skip  trans stop
      @@ write trans patch
      @@ compile filter
    | Delete (trans, Some start, stop, filter) ->
        copy trans start
      @@ skip trans stop
      @@ compile filter
    | Delete (trans, None, stop, filter) ->
        skip trans stop (compile filter)
    | Discard (trans, pos, filter) ->
        skip trans pos (compile filter)
    | Copy (trans, pos, filter) ->
        copy trans pos (compile filter)
    | SkipTo (trans, char, filter) ->
        goto trans char (compile filter)
    | CopyToEnd trans ->
        copy trans (Pos.max ~file:"") Null
    | SkipToEnd (trans, filter) ->
        skip trans (Pos.max ~file:"") (compile filter)
    | Append (trans, text, filter) ->
        copy  trans (Pos.max ~file:"")
      @@ write trans text
      @@ compile filter
    | Stop -> Null

(*
    (* Compiling in Continuation-Passing Style

       The function [compile_cps] is functionally
       equivalent to [compile], but is implemented in Continuation-Passing
       Style (CPS), implying that its calls use a constant amount of the
       OS call stack. *)

    let rec compile_cps k = function
      Insert (trans, Some pos, text, filter) ->
        compile_cps (fun v -> k (Copy (trans, pos, Write (trans, text, v)))) filter
    | Insert (trans, None, text, filter) ->
        compile_cps (fun v -> k (Write (trans, text, v))) filter
    | Overwrite (trans, pos, text, filter) ->
        compile_cps
          (fun v -> k (Copy  (trans, pos,
                    Skip  (trans, pos#shift_bytes (String.length text),
                    Write (trans, text, v))))) filter
    | Patch (trans, Some start, stop, patch, filter) ->
        compile_cps
          (fun v -> k (Copy  (trans, start,
                    Skip  (trans, stop,
                    Write (trans, patch, v))))) filter
    | Patch (trans, None, stop, patch, filter) ->
        compile_cps (fun v -> k (Skip (trans, stop, Write (trans, patch, v)))) filter
    | Delete (trans, Some start, stop, filter) ->
        compile_cps (fun v -> k (Copy (trans, start, Skip (trans, stop, v)))) filter
    | Delete (trans, None, stop, filter) ->
        compile_cps (fun v -> k (Skip (trans, stop, v))) filter
    | Discard (trans, pos, filter) ->
        compile_cps (fun v -> k (Skip (trans, pos, v))) filter
    | Copy (trans, pos, filter) ->
        compile_cps (fun v -> k (Copy (trans, pos, v))) filter
    | SkipTo (trans, char, filter) ->
        compile_cps (fun v -> k (Goto (trans, char, v))) filter
    | SkipToEnd (trans,filter) ->
        compile_cps (fun v -> k (Skip (trans, Pos.max ~file:"", v))) filter
    | Append (trans, text, filter) ->
        compile_cps (fun v -> k (Copy (trans, Pos.max ~file:"",
                              Write (trans, text, v)))) filter
    | CopyToEnd trans ->
        k (Copy (trans, Pos.max ~file:"", Null))
    | Stop -> k Null

    let compile_cps filter = compile_cps Utils.id filter
*)

    (* I/O maps and file bindings *)

    type file_name = string

    type binding = {
      lift : TransSet.t FileMap.t;
      drop : file_name TransMap.t
    }

    type io_map = {
      input     : binding;
      output    : binding;
      to_string : trans -> string
    }


    (* Pretty-printing of I/O maps (of type [io_map]) *)

    let print_trans to_string trans file =
      Printf.printf "%s: %s\n" (to_string trans) file

    let print_file to_string file tree =
      let show_trans tree = print_string (to_string tree ^ ", ")
      in Printf.printf "%s -> {" file;
      TransSet.iter show_trans tree;
      print_string "}"

    let print_bindings to_string {lift;drop} =
      print_endline " * Lift:";
      FileMap.iter (print_file to_string) lift;
      print_endline " * Drop:";
      TransMap.iter (print_trans to_string) drop

    let print_io_map io =
      print_endline "Displaying input:";
      print_bindings io.to_string io.input;
      print_endline "Displaying output:";
      print_bindings io.to_string io.output

    (* Pretty-printing of edits (of type [edit]) *)

    let string_of_copy ~offsets io trans pos =
      Printf.sprintf "*** Copy up to %s:%s:%s into %s"
        (io.to_string trans)
        (Filename.basename (TransMap.find trans io.input.drop))
        (pos#to_string ~file:true ~offsets `Byte)
        (Filename.basename (TransMap.find trans io.output.drop))

    let string_of_skip ~offsets io trans pos =
      Printf.sprintf "*** Skip up to %s:%s:%s"
        (io.to_string trans)
        (Filename.basename (TransMap.find trans io.input.drop))
        (pos#to_string ~file:true ~offsets `Byte)

    let string_of_goto io trans char =
      Printf.sprintf "*** Go to next character %s:%s:'%s'"
        (io.to_string trans)
        (Filename.basename (TransMap.find trans io.input.drop))
        (Char.escaped char)

    let string_of_write io trans text =
      Printf.sprintf "*** Write to %s:%s:\n%s"
        (io.to_string trans)
        (Filename.basename (TransMap.find trans io.output.drop))
        (if text = "" then "<empty string>" else text)

    let rec string_of_edit ~offsets io = function
      Null -> "*** Null."
    | Copy (trans, pos, edit) ->
        string_of_copy ~offsets io trans pos ^ "\n"
      ^ string_of_edit ~offsets io edit
    | Skip (trans, pos, edit) ->
        string_of_skip ~offsets io trans pos ^ "\n"
      ^ string_of_edit ~offsets io edit
    | Goto (trans, char, edit) ->
        string_of_goto io trans char ^ "\n"
      ^ string_of_edit ~offsets io edit
    | Write (trans, text, edit) ->
        string_of_write io trans text ^ "\n"
      ^ string_of_edit ~offsets io edit

    let print ~offsets io edit =
      print_endline (string_of_edit ~offsets io edit)

    (* The call [show io edits] prints the edits [edits] interpreted
       with respect to the I/O map [io]. *)

    let show ~offsets io = List.iter (print ~offsets io)

    (* Two transformations, [trans1] and [trans2], are considered
       equal by [eq_input] if their corresponding file names are equal. *)

    let eq_input io trans1 trans2 =
      let in_drop = io.input.drop
      in TransMap.(find trans1 in_drop = find trans2 in_drop)

    let eq_output io trans1 trans2 =
      let out_drop = io.output.drop
      in TransMap.(find trans1 out_drop = find trans2 out_drop)

    let eq_io io trans1 trans2 =
      eq_input io trans1 trans2 && eq_output io trans1 trans2

    (* Reducing an edit

       The value of [reduce io edit] is an edit whose operational
       semantics is the same as that of [edit] (that is, its application
       to the same input yields the same output), but is possibly
       shorter. In the following, it is helpful to keep in mind that a
       [Skip] may only change the state of the input, a [Write] may only
       change the state of the output, and [Copy] may change both. We
       also assume that successive edits apply to increasing positions
       in the source.

       The first rule says that a [Copy] or [Skip] which apply up to the
       start of the file are simply discarded.

       The following four cases deal with all combinations of two
       consecutive [Copy] or [Skip] such that they apply up to the same
       position in the same input. In these cases, the reduction is tried
       without it.

       The sixth rule states that, in case of two successive [Copy] edits,
       both from the same input file to the same output file, the second
       applying further (or at the same position), then we can get rid of
       the first [Copy] (because it is contained in the second one, as we
       assume increasing positions as a precondition to calling [reduce]).

       The seventh rule allows a [Copy] to see through an empty [Write].

       The eighth rule mandates that, in case of two successive [Skip]
       edits, both applying to the same input file, if the position does
       not decrease (assumed), then we can get rid of the first [Skip]
       (again, this is a containment rule).

       The ninth rule says that a [Skip] followed by a [Write] can
       commute: we choose the bring the [Write] on top, so the fifth or
       sixth rules may have a chance to be applied.

       The tenth rule states that a [Skip] up to the end of the input file
       is equivalent to a [Null].

       The eleventh rule says that a [Skip] followed by a [Null] can be
       ignored.

       The twelfth rule says that the writing of an empty string can be
       ignored.

       The thirteenth rule says that if two [Write] edits are composed and
       they both write to the same output file, then we can replace them
       with a single [Write] with the concatenation of their respective
       strings.

       The remaining cases apply when no reduction actually takes place.
    *)

    let rec reduce io = function
      Copy (_,pos,next) | Skip (_,pos,next) when Pos.is_min pos ->
        reduce io next

    | Copy (trans1,pos1, (Skip (trans2,pos2,sub) as next)) when pos1 = pos2 ->
        if   eq_input io trans1 trans2
        then reduce io (Copy (trans1,pos1,sub))
        else Copy (trans1, pos1, reduce io next)
    | Copy (trans1,pos1, (Copy (trans2,pos2,sub) as next)) when pos1 = pos2 ->
        if   eq_io io trans1 trans2
        then reduce io (Copy(trans1,pos1,sub))
        else Copy (trans1, pos1, reduce io next)
    | Skip (trans1,pos1, (Copy (trans2,pos2,sub) as next)) when pos1 = pos2 ->
        if   eq_input io trans1 trans2
        then reduce io (Skip(trans1,pos1,sub))
        else Skip (trans1, pos1, reduce io next)
    | Skip (trans1,pos1, (Skip (trans2,pos2,sub) as next)) when pos1 = pos2 ->
        if   eq_io io trans1 trans2
        then reduce io (Skip (trans1,pos1,sub))
        else Skip (trans1, pos1, reduce io next)

    | Copy (trans1,stop1, (Copy (trans2,_,_) as next)) ->
        if   eq_io io trans1 trans2
        then reduce io next
        else Copy (trans1, stop1, reduce io next)
    | Copy (trans,stop, Write (_,"",next)) ->
        reduce io (Copy (trans,stop,next))

    | Skip (trans1,stop1, (Skip (trans2,_,_) as next)) ->
        if   eq_input io trans1 trans2
        then reduce io next
        else Skip (trans1, stop1, reduce io next)
    | Skip (trans1,stop, Write (trans2,text,next)) ->
        reduce io (Write (trans2,text, Skip(trans1,stop,next)))
    | Skip (_,stop,_) when Pos.is_max stop -> Null
    | Skip (_,_,Null) -> Null

    | Write (_,"",next) -> reduce io next
    | Write (trans1,text1, (Write (trans2,text2,edit) as next)) ->
        if   eq_output io trans1 trans2
        then reduce io (Write (trans1, text1 ^ text2, edit))
        else Write (trans1, text1, reduce io next)

    | Copy  (trans,stop,edit) -> Copy  (trans, stop, reduce io edit)
    | Skip  (trans,stop,edit) -> Skip  (trans, stop, reduce io edit)
    | Goto  (trans,char,edit) -> Goto  (trans, char, reduce io edit)
    | Write (trans,text,edit) -> Write (trans, text, reduce io edit)
    | Null                    -> Null

    (* Merging pairs of edits

       Given two edits meant to be applied one after the other, we want
       to know if they can be merged into a single edit, so only one
       pass on the (same) input file is sufficient, instead of two. If
       so, those two mergeable edits can then be conceived as
       non-overlapping overlays reading from the same source and writing
       to the same target.

       The value of [merge part io edit1' edit2'] is an edit equivalent to
       applying edit [edit1'] and then [edit2'], or else the exception
       [Disjoint] is raised.

       The value of [norm part edit] is an edit equivalent to [edit], but
       whose kinds (that is, the transformations) have been replaced by
       their representative in the equivalence class [part]. This is not
       strictly necessary, but it enables [merge] to yield an edit (if
       exception [Disjoint] was not raised) whose atomic edits are of the
       same kind, which makes them easier to understand and debug. *)

    module Partition = Partition3.Make (Trans)

    let rec norm (part: Partition.t) =
      let open Partition in function
                           Null -> Null
      | Copy  (trans, pos,edit) -> Copy  (repr trans part,  pos, norm part edit)
      | Skip  (trans, pos,edit) -> Skip  (repr trans part,  pos, norm part edit)
      | Goto  (trans,char,edit) -> Goto  (repr trans part, char, norm part edit)
      | Write (trans, pos,edit) -> Write (repr trans part,  pos, norm part edit)

    exception Disjoint

    let rec merge (part: Partition.t) (io: io_map) edit1' edit2' =
      match edit1', edit2' with
        Copy (trans1,pos1,edit1), Skip (trans2,pos2,edit2)
      | Skip (trans1,pos1,edit1), Copy (trans2,pos2,edit2) ->
          if eq_input io trans1 trans2 then
            let part = Partition.equiv trans1 trans2 part in
            if   Pos.lt pos1 pos2
            then let edit, part = merge part io edit1 edit2' in
                 Copy (Partition.repr trans1 part, pos1, edit), part
            else if Pos.lt pos2 pos1
            then let edit, part = merge part io edit1' edit2 in
                 Copy (Partition.repr trans2 part, pos2, edit), part
            else let edit, part = merge part io edit1 edit2 in
                 Copy (Partition.repr trans2 part, pos2, edit), part
          else raise Disjoint
      | Skip (trans1,pos1,edit1), Skip (trans2,pos2,edit2) ->
          if eq_input io trans1 trans2
          then
            let part = Partition.equiv trans1 trans2 part in
            if   Pos.lt pos1 pos2
            then let edit, part = merge part io edit1 edit2' in
                 Skip (Partition.repr trans1 part, pos1, edit), part
            else if Pos.lt pos2 pos1
            then let edit, part = merge part io edit1' edit2 in
                 Skip (Partition.repr trans2 part, pos2, edit), part
            else let edit, part = merge part io edit1 edit2 in
                 Skip (Partition.repr trans2 part, pos2, edit), part
          else raise Disjoint
      | Write (trans1,text1,edit1), Write (trans2,text2,edit2) ->
          if eq_input io trans1 trans2 && eq_output io trans1 trans2
          then
            let part = Partition.equiv trans1 trans2 part
            and text = text1 ^ text2 in
            merge part io (Write (Partition.repr trans1 part, text, edit1)) edit2
          else raise Disjoint
      | Write (trans1,text1,edit1), Skip (trans2,_,_) ->
          let part = Partition.equiv trans1 trans2 part in
          let edit, part = merge part io edit1 edit2'
          in Write (Partition.repr trans1 part, text1, edit), part
      | Skip (trans1,_,_), Write (trans2,text2,edit2)->
          let part = Partition.equiv trans1 trans2 part in
          let edit, part = merge part io  edit1' edit2
          in Write (Partition.repr trans2 part, text2, edit), part
      | Null, e | e, Null -> norm part e, part
      | _ -> raise Disjoint

    (* Merging edits pairwise in a list

       The value of [merge_seq part io edits] ("merge sequentially") is a
       pair containing a list of edits whose application yields the same
       effect as that of the list [edits], assuming the same I/O map
       [io].

       The function [merge_seq] applies [merge] to the edits in
       [edits] pairwise, and moves to the next pair if no merger can
       be performed. In particular, no attempt is made at permuting
       edits to find potentially better mergers. (In that sense, what
       is done here is similar to a peep hole optimisation: the width
       of the window is two elementary edits (both of type [t]). We
       leave open the problem of finding a global optimum to this
       combinatorial problem.)

       The parameter [part] is a partition of classes of equivalence of
       edits. When [merge_seq] discovers that two edits are mergeable,
       they are registered as equivalent in [part].

       The other component of the pair computed by [merge_seq part io
       edits] is the new equivalence class after the attempt at
       merging. *)

    let rec merge_seq (part: Partition.t) (io: io_map) = function
      edit1::(edit2::rest as edit) ->
        (match merge part io edit1 edit2 with
           edit12, part -> merge_seq part io (edit12::rest)
         | exception Disjoint ->
             let edit', part' = merge_seq part io edit
             in edit1::edit', part')
      | edit -> edit, part

    (* LEXER ENGINE *)

    (* Let [pos] be a position in an input file with lexing buffer
       [lexbuf]. After a lexeme is read in the file, and stored in
       [lexbuf], the value of the call [update pos lexbuf] is the
       position after that lexeme. *)

    let update pos lexbuf : Pos.t =
      match Lexing.lexeme lexbuf with
        "\n" as nl -> pos#new_line nl
      | lexeme     -> pos#shift_bytes (String.length lexeme)

(* END OF HEADER *)
}


(* The call [scan pos action stop lexbuf] applies, as a
   side-effect, the function [action] to the first character in
   the lexing buffer [lexbuf]. Then, it updates the position [pos]
   to denote that newly read character, and proceeds to checking
   whether the action has to be applied to the next character or
   not, until the position [stop] (excluded) is reached.

   The precondition is [Pos.lt pos stop], which is checked in the
   wrapper of [scan], the function [common].

   If the lexer [scan] reaches the end of the file before it could
   apply a pending action (from an edit), it gracefully returns [pos],
   in order to enable idioms like [Copy (trans, Pos.max ~file:"",
   Null)] and [Skip (trans, Pos.max ~file:"", Null)], respectively to
   copy the remains of the input of transform [trans] to its output,
   and to skip the remains of the input.

   Technical note: the scanner does not need to roll back any lexeme
   because it assumes that the positions used to trigger the actions
   are just one column after (they are excluded from the edit). *)

rule scan pos action stop = parse
  _ as c { action c;
           let pos' = update pos lexbuf in
           if   Pos.lt pos' stop
           then scan pos' action stop lexbuf
           else pos' }
| eof    { pos }

and scan_until pos action char_stop = parse
  _ as c { action c;
           let pos' = update pos lexbuf in
           if   char_stop = c then pos'
           else scan_until pos' action char_stop lexbuf }
| eof    { pos }

{
(* POSTLUDE *)

  (* The evaluation of the call [apply ~offsets edits desc] applies
     the edits in [edits], using the I/O descriptors [desc] to
     interpret the I/O as lexing buffers (for inputs) and channels
     (for outputs).

     As the name suggests, the function [common] implements the
     commonalities between the effect of a copy edit and a skip
     edit. As a consequence, it is parameterised over the appropriate
     action ([action]) in each case: in case of a skip, nothing is
     done, whereas the current character in the input is copied to the
     output.

     The optional parameters [offsets] and [io] are only used for
     tracing the edit being applied and [io] has to be used to create
     [desc] (not checked here). *)

    let common (desc: desc) trans stop action =
      let pos, lexbuf = TransMap.find trans desc.in_desc in
      if Pos.lt pos stop then
        let pos'     = scan pos action stop lexbuf in
        let in_desc  = TransMap.add trans (pos',lexbuf) desc.in_desc
        in {desc with in_desc}
      else desc

    let rec apply edit (desc: desc) =
      let nothing _ = () in
      match edit with
        Copy (trans, stop, edit) ->
          let cout = TransMap.find trans desc.out_desc in
          let desc = common desc trans stop (output_char cout)
          in apply edit desc
      | Skip (trans, stop, edit) ->
          apply edit (common desc trans stop nothing)
      | Goto (trans, char, edit) ->
          let pos, lexbuf = TransMap.find trans desc.in_desc in
          let pos'        = scan_until pos nothing char lexbuf in
          let in_desc     = TransMap.add trans (pos',lexbuf) desc.in_desc in
          let desc        = {desc with in_desc}
          in apply edit desc
      | Write (trans, text, edit) ->
          let cout = TransMap.find trans desc.out_desc
          in output_string cout text; apply edit desc
      | Null -> flush_all ()
  end
}
