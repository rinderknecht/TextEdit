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

module type S =
  sig
    module IO_map : IO_map.S
    module Trans  = IO_map.Trans

    type t
    type edit = t

    val null  : t
    val write : Trans.t -> string      -> t -> t
    val copy  : Trans.t -> until:Pos.t -> t -> t
    val skip  : Trans.t -> until:Pos.t -> t -> t
    val goto  : Trans.t -> char        -> t -> t

    val to_buffer :
      offsets:bool -> IO_map.t -> t -> (Buffer.t, Buffer.t) Stdlib.result

    val check :
      offsets:bool -> IO_map.t -> t -> (unit, string) Stdlib.result

    val reduce :
      offsets:bool -> IO_map.t -> t -> (t, string) Stdlib.result

    (* TEMPORARY *)

    module Input = IO_map.Input
    module InMap : Map.S with type key = Input.t

    val extract_edits : IO_map.t -> t -> t InMap.t

    module TEq : Partition.S with type item = Trans.t
  end

module Make (IO_map : IO_map.S) =
  struct
    module IO_map = IO_map
    module Trans  = IO_map.Trans
    module TMap   = IO_map.TMap

    type t =
      Null
    | Copy  of Trans.t *  Pos.t * t
    | Skip  of Trans.t *  Pos.t * t
    | Write of Trans.t * string * t
    | Goto  of Trans.t *   char * t

    type edit = t

    let null                    = Null
    let copy  trans ~until edit = Copy  (trans, until,  edit)
    let skip  trans ~until edit = Skip  (trans, until,  edit)
    let write trans string edit = Write (trans, string, edit)
    let goto  trans char   edit = Goto  (trans, char,   edit)

    (* Pretty-printing of edits (of type [edit]) *)

    let sprintf = Printf.sprintf

    let print_copy buffer ~offsets io trans until =
      let trans_str = Trans.to_string trans in
      let open IO_map in
      let src, dst =
        let input, output = TMap.find trans io in
        Input.to_string input, Output.to_string output
      and region = until#compact ~file:true ~offsets `Byte in
      let s =
        sprintf "*** Copy up to %s:%s:%s into %s" trans_str src region dst
      in Buffer.add_string buffer s

    let print_skip buffer ~offsets io trans until =
      let trans_str = Trans.to_string trans
      and src = TMap.find trans io |> fst |> IO_map.Input.to_string
      and region = until#compact ~file:true ~offsets `Byte in
      let s = sprintf "*** Skip up to %s:%s:%s" trans_str src region
      in Buffer.add_string buffer s

    let print_goto buffer io trans char =
      let trans_str = Trans.to_string trans
      and src = TMap.find trans io |> fst |> IO_map.Input.to_string in
      let s = sprintf "*** Go to character %s:%s:%C"
                      trans_str src char
      in Buffer.add_string buffer s

    let print_write buffer io trans string =
      let trans_str = Trans.to_string trans
      and dst = TMap.find trans io |> snd |> IO_map.Output.to_string
      and string = if string = "" then "<empty string>" else string in
      let s = sprintf "*** Write to %s:%s:\n%s" trans_str dst string
      in Buffer.add_string buffer s

    let print_first_edit buffer ~offsets io = function
      Null ->
        Buffer.add_string buffer "*** Null."; None
    | Copy (trans, until, edit) ->
        print_copy buffer ~offsets io trans until; Some edit
    | Skip (trans, until, edit) ->
        print_skip buffer ~offsets io trans until; Some edit
    | Goto (trans, char, edit) ->
        print_goto buffer io trans char; Some edit
    | Write (trans, string, edit) ->
        print_write buffer io trans string; Some edit

    let rec print_all_edits buffer ~offsets io edits =
      match print_first_edit buffer ~offsets io edits with
        None -> ()
      | Some next_edit ->
          Buffer.add_char buffer '\n';
          print_all_edits buffer ~offsets io next_edit

    let to_buffer ~offsets io edit =
      let buffer = Buffer.create 131 in
      try
        print_all_edits buffer ~offsets io edit;
        Stdlib.Ok buffer
      with
        Not_found -> Stdlib.Error buffer

    let check ~offsets io edit =
      let buffer = Buffer.create 31 in
      let rec check map = function
        Null ->
          Stdlib.Ok ()
      | Write (_, _, edit) | Goto (_, _, edit) ->
          check map edit
      | Copy (trans, until, edit) | Skip (trans, until, edit) as e ->
          let () = Buffer.clear buffer in
          let () = print_first_edit buffer ~offsets io e |> ignore in
          let current_edit = Buffer.contents buffer in
          let map' = TMap.add trans (until, current_edit) map in
          try
            let last_pos, last_edit = TMap.find trans map in
            if   Pos.leq last_pos until
            then check map' edit
            else let msg = sprintf "Non-increasing edits:\n%s\n%s"
                                   last_edit current_edit
                 in Stdlib.Error msg
          with Not_found -> check map' edit
      in check TMap.empty edit

    (* Reductions of edits

       An edit can be sometimes _reduced_ if it contains some
       sequences of sub-edits that can be shortened or eliminated
       entirely, all the while keeping the denotational semantics
       invariant. Here, for the sake of simplicity, we choose to slide
       a window of two consecutive elementary edits on a given edit to
       determine whether a reduction applies: this is a peephole
       optimisation.

         This peephole approach is all the more limited by the fact
       that elementary edits of different transforms can be
       interleaved. Indeed, by default, two transforms with the same
       input do not actually share said input, but, instead, operate
       on copies of it. For example, if an input is a file, then two
       transforms will have each their own pointer to the file. This
       is overly conservative if two transforms can actually be merged
       into one, that is, if one pass over the input file would
       suffice. The resulting transform is called an _overlay_.

         Therefore, the function [reduce] is applied on edits that
       have already been normalised with respect to a partition of
       equivalent transforms, so each transform is the representative
       of a class (see function [normalise] below).

       In the following, it is helpful to keep in mind that

         * [Skip] may only change the state of the input,
         * [Write] may only change the state of the output, and
         * [Copy] may change both.

       In particular, this entails the following, assuming the
       positions are non-decreasing (see function [check]).

       No-operations:

         * an empty [Write] can be removed, as well as a [Copy] or a
           [Skip] applying until the beginning of their inputs.

       Reductions to [Null]:

         * a [Skip] until the end of its input can be replaced by a
           [Null] and any following edits discarded;

         * a [Skip] followed by a [Null] can be removed.

       Two similar edits:

         * the first of two [Copy] can be removed if their inputs and
           outputs are the same;

         * the first of two [Skip] can be removed if their inputs are
           the same;

         * two succesive [Write] can be reduced into one if their
           output is the same;

       Two different edits:

         * a [Skip] can always commute with a [Write]: making them
           commute is a systematic way may help bring about reduction
           cases [Write]+[Write] or [Skip]+[Skip] (see above).

         * a [Skip] in combination with a [Copy] (or vice-versa) may
           be removed if their inputs and their ending positions are
           the same.

         * the combinations [Copy]+[Write] or [Write]+[Copy] cannot be
           reduced.

       NOTE: We assume in the definition of [reduce] that elementary
       edits apply to non-decreasing positions. See precondition
       [check]. *)

    let rec reduce io = function
    (* A [Copy] or [Skip] which apply up to the start of their input
       are simply discarded. *)
      Copy (_, pos, next) | Skip (_, pos, next) when Pos.is_min pos ->
        reduce io next

    (* Two consecutive [Copy] or [Skip] for the same transform such
       that they apply up to the same position: the [Skip] is
       discarded. *)

    | Copy (trans1, pos1, Skip (trans2, pos2, sub))
        when Trans.equal trans1 trans2 && Pos.equal pos1 pos2 ->
        Copy (trans1, pos1, reduce io sub)

    | Skip (trans1, pos1, Copy (trans2, pos2, sub))
        when Trans.equal trans1 trans2 && Pos.equal pos1 pos2 ->
        Copy (trans2, pos2, reduce io sub)

    (* In case of two successive [Copy] edits, both from the same
       transform, the second applying further (or at the same
       position), then we can get rid of the first [Copy] (because it
       is contained in the second one, as we assume increasing
       positions as a precondition to calling [reduce] -- see call to
       [check]). *)

    | Copy (trans1, _, Copy (trans2, pos2, sub))
        when Trans.equal trans1 trans2 ->
        Copy (trans2, pos2, reduce io sub)

    (* In case of two successive [Skip] edits, both from the same
       transformation, and if the positions do not decrease
       (precondition), then we can get rid of the first [Skip]. *)

    | Skip (trans1, _, (Skip (trans2, pos2, _) as next))
        when Trans.equal trans1 trans2 ->
        Skip (trans2, pos2, reduce io next)

    (* In the cae of two succesive [Write] edits, both from the same
       transformation, and if the positions do not decrease
       (precondition), we can replace them with a single [Write] with
       the concatenation of their respective strings. *)

    | Write (trans1, text1, Write (trans2, text2, edit))
        when Trans.equal trans1 trans2 ->
        reduce io (Write (trans1, text1 ^ text2, edit))

    (* A [Skip] followed by a [Write] can always commute: we choose to
       bring the [Write] on top to enable its removal by the recursive
       call if the text to write is empty (see rule below). *)

    | Skip (trans1, pos1, Write (trans2, text, next)) ->
        reduce io (Write (trans2, text, Skip (trans1, pos1, next)))

    (* A [Skip] till the end of the input is equivalent to a
       [Null]. *)

    | Skip (_, stop, _) when Pos.is_max stop -> Null

    (* A [Skip] followed by a [Null] is equivalent to a [Null]. *)

    | Skip (_, _, Null) -> Null

    (* The writing of an empty string can be ignored. *)

    | Write (_, "", next) -> reduce io next

    (* The remaining cases apply when no reduction actually takes
       place. *)

    | Copy  (trans, stop, edit) -> Copy  (trans, stop, reduce io edit)
    | Skip  (trans, stop, edit) -> Skip  (trans, stop, reduce io edit)
    | Goto  (trans, char, edit) -> Goto  (trans, char, reduce io edit)
    | Write (trans, text, edit) -> Write (trans, text, reduce io edit)
    | Null                      -> Null

    let reduce ~offsets io edit =
      match check ~offsets io edit with
        Stdlib.Error _ as err -> err
      | Ok () -> Ok (reduce io edit)

    (* Two transformations, [trans1] and [trans2], are considered
       equal by [eq_input] if, and only if, their corresponding
       inputs are equal. *)
(*
    let eq_input io trans1 trans2 =
      try
        let input_1 = TMap.find trans1 io |> fst
        and input_2 = TMap.find trans2 io |> fst
        in IO_map.Input.equal input_1 input_2
      with Not_found -> false

    let eq_output io trans1 trans2 =
      try
        let output_1 = TMap.find trans1 io |> snd
        and output_2 = TMap.find trans2 io |> snd
        in IO_map.Output.equal output_1 output_2
      with Not_found -> false

    let eq_io io trans1 trans2 =
      eq_input io trans1 trans2 && eq_output io trans1 trans2
*)
  (* Merging pairs of edits

   Given two edits meant to be applied one after the other, we want to
   know if they can be merged into a single edit, so only one pass on
   the (same) input file is sufficient, instead of two. If so, those
   two mergeable edits can then be conceived as non-overlapping
   overlays reading from the same source and writing to the same
   target.

   The value of [merge eqc io edit1' edit2'] is an edit equivalent to
   applying edit [edit1'] and then [edit2'], or else the exception
   [Disjoint] is raised.

   The value of [norm eqc edit] is an edit equivalent to [edit], but
   whose kinds (that is, the transformations) have been replaced by
   their representative in the equivalence class [eqc]. This is not
   strictly necessary, but it enables [merge] to yield an edit (if
   exception [Disjoint] was not raised) whose atomic edits are of the
   same kind, which makes them easier to understand and debug. *)

    module TEq = Partition.Make (Trans)

    module Input = IO_map.Input
    module InMap = Map.Make (Input)
    module TSet  = Set.Make (Trans)

    let sort_inputs io_map : TSet.t InMap.t =
      let apply trans (input, _) in_map =
        let t_set =
          match InMap.find_opt input in_map with
            Some t_set -> TSet.add trans t_set
          | None -> TSet.singleton trans
        in InMap.add input t_set in_map
      in TMap.fold apply io_map InMap.empty

    let rec filter t_set = function
      Null -> Null
    | Copy (trans, loc, edit) ->
        if TSet.mem trans t_set then
          Copy (trans, loc, filter t_set edit)
        else filter t_set edit
    | Skip (trans, loc, edit) ->
        if TSet.mem trans t_set then
         Skip (trans, loc, filter t_set edit)
        else filter t_set edit
    | Goto (trans, char, edit) ->
        if TSet.mem trans t_set then
          Goto (trans, char, filter t_set edit)
        else filter t_set edit
    | Write (trans, loc, edit) ->
        if TSet.mem trans t_set then
          Write (trans, loc, filter t_set edit)
        else filter t_set edit

    let extract_edits io_map edit : edit InMap.t =
      let apply input t_set =
        InMap.add input @@ filter t_set edit in
      InMap.fold apply (sort_inputs io_map) InMap.empty

(*
    let rec norm (part : TEq.partition) =
      function
        Null -> Null
      | Copy (trans, loc, edit) ->
          Copy  (TEq.repr trans part, loc, norm part edit)
      | Skip (trans, loc, edit) ->
          Skip (TEq.repr trans part, loc, norm part edit)
      | Goto (trans, char, edit) ->
          Goto (TEq.repr trans part, char, norm part edit)
      | Write (trans, loc, edit) ->
          Write (TEq.repr trans part, loc, norm part edit)

    exception Disjoint

    let rec merge (part : TEq.partition) (io : IO_map.t) edit1' edit2' =
      match edit1', edit2' with
        Copy (trans1, loc1, edit1), Skip (trans2, loc2, edit2)
      | Skip (trans1, loc1, edit1), Copy (trans2, loc2, edit2) ->
          if   eq_input io trans1 trans2
          then let part = TEq.equiv trans1 trans2 part in
               if   Pos.lt loc1 loc2
               then let edit, part = merge part io edit1 edit2' in
                    Copy (TEq.repr trans1 part, loc1, edit), part
               else if Pos.lt loc2 loc1
               then let edit, part = merge part io edit1' edit2 in
                    Copy (TEq.repr trans2 part, loc2, edit), part
               else let edit, part = merge part io edit1 edit2 in
                    Copy (TEq.repr trans2 part, loc2, edit), part
          else raise Disjoint
      | Skip (trans1, loc1, edit1), Skip (trans2, loc2, edit2) ->
          if   eq_input io trans1 trans2
          then let part = TEq.equiv trans1 trans2 part in
               if   Pos.lt loc1 loc2
               then let edit, part = merge part io edit1 edit2' in
                    Skip (TEq.repr trans1 part, loc1, edit), part
               else if Pos.lt loc2 loc1
               then let edit, part = merge part io edit1' edit2 in
                    Skip (TEq.repr trans2 part, loc2, edit), part
               else let edit, part = merge part io edit1 edit2 in
                    Skip (TEq.repr trans2 part, loc2, edit), part
          else raise Disjoint
      | Write (trans1, text1, edit1), Write (trans2, text2, edit2) ->
          if   eq_input io trans1 trans2 && eq_output io trans1 trans2
          then let part = TEq.equiv trans1 trans2 part
               and text = text1 ^ text2
               in merge part io
                        (Write (TEq.repr trans1 part, text, edit1)) edit2
          else raise Disjoint
      | Write (trans1, text1, edit1), Skip (trans2, _, _) ->
          let part = TEq.equiv trans1 trans2 part in
          let edit, part = merge part io edit1 edit2'
          in Write (TEq.repr trans1 part, text1, edit), part
      | Skip (trans1, _, _), Write (trans2, text2, edit2)->
          let part = TEq.equiv trans1 trans2 part in
          let edit, part = merge part io  edit1' edit2
          in Write (TEq.repr trans2 part, text2, edit), part
      | Null, e | e, Null ->
          norm part e, part
      | _ -> raise Disjoint
 *)

  end
