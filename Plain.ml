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

    type message = string
    type error = string * message

    val to_string :
      offsets:bool -> IO_map.t -> t -> (string, error) Stdlib.result

    val check :
      offsets:bool -> IO_map.t -> t -> (unit, message) Stdlib.result

    val reduce :
      offsets:bool -> IO_map.t -> t -> (t, message) Stdlib.result
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

    type message = string
    type error = string * message

    exception Error of string

    let print_copy buffer ~offsets io trans until =
      let trans_str = Trans.to_string trans in
      let open IO_map in
      let src =
        match TMap.find_opt trans io.input.drop with
          Some input -> Input.to_string input
        | None ->
           let msg =
             sprintf "Copy: No input for transform %s." trans_str
           in raise (Error msg)
      and dst =
        match TMap.find_opt trans io.output.drop with
          Some output -> Output.to_string output
        | None ->
            let msg =
              sprintf "Copy: No input for transform %s." trans_str
            in raise (Error msg)
      and region = until#compact ~file:true ~offsets `Byte in
      let s =
        sprintf "*** Copy up to %s:%s:%s into %s" trans_str src region dst
      in Buffer.add_string buffer s

    let print_skip buffer ~offsets io trans until =
      let trans_str = Trans.to_string trans in
      let src =
        let open IO_map in
        match TMap.find_opt trans io.input.drop with
          Some input -> Input.to_string input
        | None ->
            let msg =
              sprintf "Skip: No input for transform %s." trans_str
            in raise (Error msg)
      and region = until#compact ~file:true ~offsets `Byte in
      let s = sprintf "*** Skip up to %s:%s:%s" trans_str src region
      in Buffer.add_string buffer s

    let print_goto buffer io trans char =
      let trans_str = Trans.to_string trans in
      let src =
        let open IO_map in
        match TMap.find_opt trans io.input.drop with
          Some input -> Input.to_string input
        | None ->
            let msg =
              sprintf "Goto: No input for transform %s." trans_str
            in raise (Error msg) in
      let s = sprintf "*** Go to character %s:%s:'%s'"
                      trans_str src (Char.escaped char)
      in Buffer.add_string buffer s

    let print_write buffer io trans string =
      let trans_str = Trans.to_string trans in
      let dst =
        let open IO_map in
        match TMap.find_opt trans io.output.drop with
          Some output -> Output.to_string output
        | None ->
            let msg =
              sprintf "Write: No input for transform %s." trans_str
            in raise (Error msg) in
      let string = if string = "" then "<empty string>" else string in
      let s = sprintf "*** Write to %s:%s:\n%s" trans_str dst string
      in Buffer.add_string buffer s

    let print_first_edit buffer ~offsets io = function
      Null ->
        Buffer.add_string buffer "*** Null."; None
    | Copy (trans, until, edit) ->
        print_copy buffer ~offsets io trans until;
        Some edit
    | Skip (trans, until, edit) ->
        print_skip buffer ~offsets io trans until;
        Some edit
    | Goto (trans, char, edit) ->
        print_goto buffer io trans char;
        Some edit
    | Write (trans, string, edit) ->
        print_write buffer io trans string;
        Some edit

    let rec print_all_edits buffer ~offsets io edits =
      match print_first_edit buffer ~offsets io edits with
        None -> ()
      | Some next_edit ->
          Buffer.add_char buffer '\n';
          print_all_edits buffer ~offsets io next_edit

    let to_string ~offsets io edit =
      let buffer = Buffer.create 131 in
      try
        print_all_edits buffer ~offsets io edit;
        Stdlib.Ok (Buffer.contents buffer)
      with
        Error msg -> Stdlib.Error (Buffer.contents buffer, msg)

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

    (* Two transformations, [trans1] and [trans2], are considered
       equal by [eq_input] if, and only if, their corresponding
       handles are equal. *)

    let eq_input io trans1 trans2 =
      try
        let in_drop = IO_map.(io.input.drop) in
        let input_1 = TMap.find trans1 in_drop
        and input_2 = TMap.find trans2 in_drop
        in IO_map.Input.equal input_1 input_2
      with Not_found -> false

    let eq_output io trans1 trans2 =
      try
        let out_drop = IO_map.(io.output.drop) in
        let output_1 = TMap.find trans1 out_drop
        and output_2 = TMap.find trans2 out_drop
        in IO_map.Output.equal output_1 output_2
      with Not_found -> false

    let eq_io io trans1 trans2 =
      eq_input io trans1 trans2 && eq_output io trans1 trans2

    (* Reducing an edit by peep-hole.

       In the following, it is helpful to keep in mind that

         * [Skip] may only change the state of the input,
         * [Write] may only change the state of the output, and
         * [Copy] may change both.

       In particular, this entails the following, assuming the
       positions are non-decreasing (see function [check]).

       No-operations:

         * a empty [Write] can be removed, as well as a [Copy] or
           a [Skip] applying until the beginning of their inputs;

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
           the same (non-overlapping overlay).

         * the combinations [Copy]+[Write] or [Write]+[Copy] cannot be
           reduced. *)

    let rec reduce io = function
      (* A [Copy] or [Skip] which apply up to the start of their handle
         are simply discarded. *)
      Copy (_, pos, next) | Skip (_, pos, next) when Pos.is_min pos ->
        reduce io next

      (* Two consecutive [Copy] or [Skip] such that they apply up to
         the same position in the same input (a non-overlapping
         overlay): the [Skip] is discarded. *)

    | Copy (trans1, pos1, (Skip (trans2, pos2, sub) as next)) when pos1 = pos2 ->
        if   eq_input io trans1 trans2
        then reduce io (Copy (trans1, pos1, sub))
        else Copy (trans1, pos1, reduce io next)

    | Skip (trans1, pos1, (Copy (trans2, pos2, sub) as next)) when pos1 = pos2 ->
        if   eq_input io trans1 trans2
        then reduce io next
        else Skip (trans1, pos1, reduce io next)

    (* In case of two successive [Copy] edits, both from the same
       input to the same output, the second applying further (or at
       the same position), then we can get rid of the first [Copy]
       (because it is contained in the second one, as we assume
       increasing positions as a precondition to calling [reduce] --
       see call to [check]). *)

    | Copy (trans1, stop1, (Copy (trans2, _, _) as next)) ->
        if   eq_io io trans1 trans2
        then reduce io next
        else Copy (trans1, stop1, reduce io next)

    (* In case of two successive [Skip] edits, both applying to the
       same input file, if the position does not decrease (assumed --
       see call to [check]), then we can get rid of the first [Skip]
       (again, this is a containment rule). *)

    | Skip (trans1, stop1, (Skip (trans2, _, _) as next)) ->
        if   eq_input io trans1 trans2
        then reduce io next
        else Skip (trans1, stop1, reduce io next)

    (* A [Skip] followed by a [Write] can commute: we choose to bring
       the [Write] on top. *)

    | Skip (trans1, stop, Write (trans2, text, next)) ->
        reduce io (Write (trans2, text, Skip (trans1, stop, next)))

    (* A [Skip] till the end of the input is equivalent to a
       [Null]. *)

    | Skip (_, stop, _) when Pos.is_max stop -> Null

    (* A [Skip] followed by a [Null] can be ignored. *)

    | Skip (_, _, Null) -> Null

    (* The writing of an empty string can be ignored. *)

    | Write (_, "", next) -> reduce io next

    (* If two [Write] edits are composed and they both write to the
       same output file, then we can replace them with a single
       [Write] with the concatenation of their respective strings. *)

    | Write (trans1, text1, (Write (trans2, text2, edit) as next)) ->
        if   eq_output io trans1 trans2
        then reduce io (Write (trans1, text1 ^ text2, edit))
        else Write (trans1, text1, reduce io next)

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
  end
