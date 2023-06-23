(** {1 Goal Manager} *)

module D = Imandra_document.Document
open Printf

type t = {
  name: string;
  section: string option;
  desc: string;
  owner: owner option;
  status: status;
  expected: expected;
  mode: mode;
  idx: int;
  hints: (unit -> Imandra_surface.Uid.t Imandra_surface.Hints.t) option;
  model_candidates: string list;
  upto: Imandra_syntax.Logic_ast.upto option;
}

and mode =
  | For_all
  | Exists

and status =
  | Open of { assigned_to: owner option }
  | Closed of {
      timestamp: float;
      duration: float;
      result: [ `Verify of Verify.t | `Instance of Instance.t ];
    }
  | Error of string

and expected =
  | True
  | False
  | Unknown

and owner = string

and id = string * string option
(* name, section *)

type goal = t

let id_of_goal (g : t) = g.name, g.section

let expected_to_string (e : expected) : string =
  match e with
  | True -> "true"
  | False -> "false"
  | Unknown -> "unknown"

let expected_of_string (s : string) : expected option =
  match s with
  | "true" -> Some True
  | "false" -> Some False
  | "unknown" -> Some Unknown
  | _ -> None

let mode_to_string (m : mode) : string =
  match m with
  | For_all -> "for_all"
  | Exists -> "exists"

let mode_of_string (s : string) : mode option =
  match s with
  | "for_all" -> Some For_all
  | "exists" -> Some Exists
  | _ -> None

module State = struct
  type t = {
    goals: (id, goal) Hashtbl.t;
    focus: goal option;
    section: string option;
    max_idx: int;
    t_begin: float;
  }

  let state : t ref =
    ref
      {
        goals = Hashtbl.create 100;
        focus = None;
        section = None;
        max_idx = 0;
        t_begin = 0.;
      }

  module Set = struct
    let goals x = state := { !state with goals = x }

    let focus x = state := { !state with focus = x }

    let section x = state := { !state with section = x }

    let max_idx x = state := { !state with max_idx = x }

    let t_begin x = state := { !state with t_begin = x }
  end

  let install (g : goal) =
    Set.max_idx (!state.max_idx + 1);
    Hashtbl.add !state.goals (id_of_goal g) g

  let update (g : goal) = Hashtbl.replace !state.goals (id_of_goal g) g

  let list_of_goals () =
    let cmp (_, g) (_, g') = Stdlib.compare g.idx g'.idx in
    let goals = CCHashtbl.to_list !state.goals in
    List.fast_sort cmp goals

  let clear () =
    state :=
      {
        goals = Hashtbl.create 100;
        focus = None;
        section = None;
        max_idx = 0;
        t_begin = Unix.gettimeofday ();
      }
end

let clear = State.clear

module Section = struct
  let start x =
    State.Set.section (Some x);
    State.Set.t_begin (Unix.gettimeofday ())

  let stop _x = State.Set.section None

  let get () = !State.state.State.section

  let to_string () =
    match get () with
    | None -> "<global>"
    | Some s -> s
end

let make ?section ?owner ?(expected = Unknown) ?(mode = For_all) ?hints ?upto
    ?(model_candidates = []) ~desc ~name () : t =
  {
    name;
    section =
      (match section with
      | None -> !State.state.State.section
      | x -> x);
    desc;
    owner;
    status = Open { assigned_to = owner };
    expected;
    mode;
    idx = State.(!state.max_idx);
    model_candidates;
    hints;
    upto;
  }

let install (g : t) : unit =
  State.(install g);
  State.(Set.focus (Some g))

let init ?section ?owner ?expected ?mode ?hints ?upto ?model_candidates ~desc
    ~name () : unit =
  let g =
    make ?section ?owner ?expected ?mode ?hints ?upto ?model_candidates ~desc
      ~name ()
  in
  install g

let focus () = State.(!state.focus)

let close_goal ?hints g =
  let timestamp = Unix.gettimeofday () in
  let finalise g =
    State.update g;
    State.Set.focus None;
    g
  in
  let hints =
    match hints with
    | None -> g.hints |> CCOption.map (fun mk -> mk ())
    | Some _ -> hints
  in
  try
    let r =
      match g.mode with
      | For_all -> `Verify (Verify.top ?hints ?upto:g.upto g.name)
      | Exists -> `Instance (Instance.top ?hints ?upto:g.upto g.name)
    in
    let duration = Unix.gettimeofday () -. timestamp in
    let status = Closed { timestamp; duration; result = r } in
    let g = { g with status } in
    finalise g
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    let s =
      CCFormat.sprintf "Verification error: (%a) is %s undefined?"
        (Imandra_syntax.Util_err.pp_exn ~bt ?input:None)
        e g.name
    in
    let g = { g with status = Error s } in
    Printf.printf "%s\n%!" s;
    finalise g

let close ?hints ?name () =
  match name with
  | None ->
    (match focus () with
    | Some g -> close_goal ?hints g |> ignore
    | _ -> Error.unsupportedf ~loc:Iloc.none "No goal under focus")
  | Some g -> close_goal ?hints g |> ignore

let verify_ ?hints = close ?hints

let all () = State.(!state.goals) |> CCHashtbl.to_list

(* Report *)
exception Write_to_file of string * exn

let write_to_file filename s =
  try
    let cout = open_out filename in
    Printf.fprintf cout "%s\n" s;
    close_out cout
  with e -> raise (Write_to_file (filename, e))

let imandra_custom_css : string = Imandra_goals_consts_.css

let imandra_custom_logo : string = Imandra_goals_consts_.logo

module Report = struct
  type progress =
    | Complete of int
    | Partial of int * int

  let ok_green = D.p ~a:[ D.A.green; D.A.cls "text-success" ] "✔"

  let bad_red = D.p ~a:[ D.A.yellow; D.A.cls "text-danger" ] "×"

  let doc_of_progress = function
    | Complete k ->
      D.block
        [
          ok_green;
          D.s_f "Verification complete: %d out of %d goals verified" k k;
        ]
    | Partial (k, k') ->
      D.block
        [
          bad_red;
          D.s_f "Verification incomplete: %d out of %d goals verified" k k';
        ]

  module Stat = struct
    type t = {
      proved: int;
      total: int;
      time: float;
    }

    let to_doc self : D.t =
      let { proved; total; time } = self in
      D.record
        [
          "Proved", D.int proved;
          "Total", D.int total;
          "Time", D.s_f "%.2fs" time;
        ]
  end

  module Digest = struct
    type t = {
      progress: progress;
      elapsed_time: float;
      section_name: string;
      (* report_file    : string; *)
      goal_names: string list;
    }

    (* TODO: use https://getbootstrap.com/docs/4.5/components/progress/ *)
    let percent_of_progress = function
      | Complete _ -> 100
      | Partial (a, b) ->
        let q = float_of_int a /. float_of_int b *. 100. in
        int_of_float (floor q)

    let doc_of_digest (d : t) : D.t =
      let p = percent_of_progress d.progress in
      D.record
        [
          "Section name", D.s d.section_name;
          "Progress", D.s_f "%d %%" p;
          "Elapsed time", D.s_f "%.2fs" d.elapsed_time;
          ( "Goal names",
            D.fold ~summary:"goal names" ~folded_by_default:true
            @@ D.list_of D.p d.goal_names );
        ]

    let get_stats (hs : t list) : Stat.t =
      let proved = ref 0 in
      let total = ref 0 in
      let time = ref 0. in
      let f = function
        | Complete k ->
          proved := !proved + k;
          total := !total + k
        | Partial (a, b) ->
          proved := !proved + a;
          total := !total + b
      in
      List.iter
        (fun d ->
          f d.progress;
          time := !time +. d.elapsed_time)
        hs;
      Stat.{ proved = !proved; total = !total; time = !time }
  end

  let build_id () =
    match Version.build_commit_id with
    | Some s -> D.s_f "- Build commit ID %s" (String.trim s)
    | None -> D.empty

  let in_header ~status ~content ~stats : D.t =
    let d2 =
      D.section "Imandra Verification Report"
        [
          Stat.to_doc stats;
          doc_of_progress status;
          content;
          D.block
            [ D.s_f "Verified with Imandra v%s" Version.version; build_id () ];
        ]
    in
    let logo = D.html (D.Unsafe_.html_of_string imandra_custom_logo) in
    D.block [ logo; d2 ]

  let status_marker g : D.t =
    let open Verify in
    let open Instance in
    match g.status, g.expected with
    | Closed { result = `Verify (V_refuted _); _ }, True
    | Closed { result = `Instance (I_unsat _); _ }, True
    | Closed { result = `Verify (V_proved _); _ }, False
    | Closed { result = `Instance (I_sat _); _ }, False ->
      bad_red
    | Closed { result = `Verify (V_proved _); _ }, True
    | Closed { result = `Instance (I_sat _); _ }, True
    | Closed { result = `Verify (V_refuted _); _ }, False
    | Closed { result = `Instance (I_unsat _); _ }, False ->
      ok_green
    | Error _, _ -> D.p ~a:[ D.A.red; D.A.cls "text-danger" ] "ERROR"
    | _ -> D.p ~a:[ D.A.yellow; D.A.cls "text-warning" ] "?"

  let item ?(compressed = false) (g : t) : D.t =
    Debug.tracef (fun k -> k "Working on %s\n%!" g.name);
    let module V = Verify in
    let module I = Instance in
    let sd =
      match g.status with
      | Open _ -> D.bold @@ D.s "Goal is open"
      | Error s -> D.bold @@ D.s_f "Error: %s" s
      | Closed { result; _ } ->
        let mkproof p =
          match p with
          | Some p when not compressed ->
            D.fold ~folded_by_default:true ~summary:"proof" p
          | _ -> D.empty
        and mk_model m =
          if compressed then
            D.empty
          else
            D.fold ~folded_by_default:false ~summary:"model"
              (Term.Model.to_doc m)
        in
        (match result with
        | `Verify (V.V_proved { proof = p; _ }) ->
          D.v_block [ D.bold @@ D.s "Proved"; mkproof p ]
        | `Verify (V.V_proved_upto { upto; _ }) ->
          D.bold @@ D.s_f "Proved up to %a" Event.pp_upto upto
        | `Verify (V.V_refuted { proof = p; model; _ }) ->
          D.v_block [ D.bold @@ D.s "Refuted"; mk_model model; mkproof p ]
        | `Verify (V.V_unknown { proof = p; _ }) ->
          D.v_block [ D.bold @@ D.s "Unknown"; mkproof p ]
        | `Instance (I.I_sat { proof = p; model; _ }) ->
          D.v_block
            [ D.bold @@ D.s "Instance exists"; mk_model model; mkproof p ]
        | `Instance (I.I_unsat_upto { upto; _ }) ->
          D.bold @@ D.s_f "Instance doesn't exist up to %a" Event.pp_upto upto
        | `Instance (I.I_unsat { proof = p; _ }) ->
          D.v_block [ D.bold @@ D.s "Instance doesn't exist"; mkproof p ]
        | `Instance (I.I_unknown { proof = p; _ }) ->
          D.v_block [ D.bold @@ D.s "Unknown"; mkproof p ])
    in
    D.record
      [
        "Status", status_marker g;
        "VG", D.bold (D.p g.name);
        "Description", D.p g.desc;
        "Result", sd;
      ]

  let progress_of_oc goals =
    let opens =
      List.filter
        (fun (_, g) ->
          match g.status with
          | Open _ | Error _ -> true
          | _ -> false)
        goals
    and closed =
      List.filter
        (fun (_, g) ->
          match g.status with
          | Closed _ -> true
          | _ -> false)
        goals
    in
    let k_o = List.length opens in
    let k_c = List.length closed in
    let proved =
      CCList.count
        (fun (_, g) ->
          match g.status, g.expected with
          | ( Closed
                {
                  result =
                    `Verify (Verify.V_proved _) | `Instance (Instance.I_sat _);
                  _;
                },
              True )
          | ( Closed
                {
                  result =
                    ( `Verify (Verify.V_refuted _)
                    | `Instance (Instance.I_unsat _) );
                  _;
                },
              False ) ->
            true
          | _ -> false)
        closed
    in
    let k_o = k_o + k_c - proved in
    if k_o = 0 then
      Complete k_c
    else
      Partial (proved, k_o + proved)

  let by_section ~compressed l : D.t * Stat.t =
    let goal_sections =
      CCList.group_by
        ~hash:(fun (_id, g) -> Hashtbl.hash g.section)
        ~eq:(fun (_id1, g1) (_id2, g2) ->
          CCEqual.option String.equal g1.section g2.section)
        l
    in

    (* document for this section (all docs are in the same section) *)
    let doc_of_sec ~section (goals : (_ * goal) list) : D.t * Digest.t =
      let progress = progress_of_oc goals in

      (* compute digest *)
      let elapsed_time =
        List.fold_left
          (fun n (_, g) ->
            match g.status with
            | Closed { duration; _ } -> n +. duration
            | _ -> n)
          0. goals
      in
      let digest =
        Digest.
          {
            progress;
            elapsed_time;
            section_name = section;
            goal_names = List.map (fun (_, g) -> g.name) goals;
          }
      in

      ( D.section section
          [
            doc_of_progress progress;
            Digest.doc_of_digest digest;
            D.list_of (fun (_, x) -> item ~compressed x) goals;
          ],
        digest )
    in

    let docs, digests =
      List.map
        (fun goals ->
          let sec =
            match goals with
            | (_, g) :: _ -> g.section
            | _ -> None
          in
          let sec = CCOpt.get_or ~default:"<no section>" sec in
          let doc, digest = doc_of_sec ~section:sec goals in
          (sec, doc), digest)
        goal_sections
      |> List.split
    in
    (* sort by section *)
    let docs =
      List.sort (fun (s1, _) (s2, _) -> CCString.compare_natural s1 s2) docs
    in

    let stats = Digest.get_stats digests in
    D.list (List.map snd docs), stats

  let doc_to_html ?custom_css (doc : D.t) : string =
    let module H = Tyxml.Html in
    let module DH = Imandra_document_tyxml in
    let mapper =
      {
        DH.Mapper.default with
        (* just force size of first column of records *)
        DH.Mapper.attr_row =
          (fun self ~row ~col d ->
            let d = DH.Mapper.default.DH.Mapper.attr_row self ~row ~col d in
            if col > 0 then
              d
            else
              (H.a_class [ "col-3" ] :> Html_types.td_attrib H.attrib) :: d);
      }
    in

    let headers =
      match custom_css with
      | None -> []
      | Some c -> [ H.style [ H.txt c ] ]
    in
    DH.Mapper.run_doc ~headers ~title:"Imandra report" mapper doc
    |> DH.string_of_html_doc

  let top ?custom_css ~compressed ~filename () =
    try
      let l = State.list_of_goals () in
      let gs, stats = by_section ~compressed l in
      let progress = progress_of_oc l in
      let doc = in_header ~status:progress ~content:gs ~stats in
      let html = doc_to_html ?custom_css doc in
      write_to_file (filename ^ ".html") html;
      (* TODO: put on top of file?
         let time = Unix.gettimeofday () -. State.(!state.t_begin) in
         let goal_names = List.map (fun (_, g) -> g.name) all in
      *)
      printf "Report written to %s.\n" filename
    with e ->
      Error.unsupportedf ~loc:Iloc.none
        "Error writing report file (%s).\nException: %s" filename
        (Printexc.to_string e)
end

let report ?(custom_css = imandra_custom_css) ?(compressed = false) filename =
  Report.top ~custom_css ~compressed ~filename ()

module Encode (E : Decoders.Encode.S) = struct
  [@@@warning "-40"]

  open E

  let obj_opt kvs =
    kvs
    |> CCList.filter_map (fun (k, v) -> v |> CCOption.map (fun v -> k, v))
    |> obj

  let req k enc v = k, Some (enc v)

  let opt k enc v = k, CCOption.map enc v

  let upto (upto : Imandra_syntax.Logic_ast.upto) : value =
    match upto with
    | Upto_steps i -> list value [ string "steps"; int i ]
    | Upto_bound i -> list value [ string "bound"; int i ]

  let result_verify (v : Verify.t) : value =
    let kvs =
      match v with
      | V_proved _ -> [ req "v" string "proved" ]
      | V_proved_upto u ->
        [ req "v" string "proved_upto"; req "upto" upto u.upto ]
      | V_refuted _ -> [ req "v" string "refuted" ]
      | V_unknown _ -> [ req "v" string "unknown" ]
    in
    obj_opt (req "ty" string "verify" :: kvs)

  let result_instance (i : Instance.t) : value =
    let kvs =
      match i with
      | I_unsat _ -> [ req "v" string "unsat" ]
      | I_unsat_upto u ->
        [ req "v" string "unsat_upto"; req "upto" upto u.upto ]
      | I_sat _ -> [ req "v" string "sat" ]
      | I_unknown _ -> [ req "v" string "unknown" ]
    in
    obj_opt (req "ty" string "instance" :: kvs)

  let result (r : [ `Verify of Verify.t | `Instance of Instance.t ]) : value =
    match r with
    | `Verify v -> result_verify v
    | `Instance i -> result_instance i

  let status (s : status) : value =
    obj_opt
      (match s with
      | Open { assigned_to } ->
        [ req "ty" string "open"; opt "assigned_to" string assigned_to ]
      | Closed { timestamp; duration; result = result' } ->
        [
          req "ty" string "closed";
          req "timestamp" float timestamp;
          req "duration" float duration;
          req "result" result result';
        ]
      | Error msg -> [ req "ty" string "error"; req "msg" string msg ])

  let t (t : t) : value =
    obj_opt
      [
        req "name" string t.name;
        opt "section" string t.section;
        req "desc" string t.desc;
        opt "owner" string t.owner;
        req "status" status t.status;
        req "expected" (of_to_string expected_to_string) t.expected;
        req "mode" (of_to_string mode_to_string) t.mode;
        req "idx" int t.idx;
        (* hints *)
        (* model_candidates *)
        opt "upto" upto t.upto;
      ]

  let goals (gs : (id * t) list) : value =
    obj (gs |> CCList.map (fun ((name, _section), g) -> (name, t g)))
end
