(** {1 Goal Manager} *)

type t = {
  name: string;
  section: string option;
  desc: string;
  owner: owner option;
  status: status;
  expected: expected;
  mode: mode;
  idx: int;
  hints: Imandra_surface.Uid.t Imandra_surface.Hints.t option;
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

val init :
  ?section:string ->
  ?owner:owner ->
  ?expected:expected ->
  ?mode:mode ->
  ?hints:Imandra_surface.Uid.t Imandra_surface.Hints.t ->
  ?upto:Imandra_syntax.Logic_ast.upto ->
  desc:string ->
  name:string ->
  unit ->
  unit
(** Start a new goal *)

val close_goal : ?hints:Imandra_surface.Uid.t Imandra_surface.Hints.t -> t -> t

val close :
  ?hints:Imandra_surface.Uid.t Imandra_surface.Hints.t ->
  ?name:t ->
  unit ->
  unit

val verify_ :
  ?hints:Imandra_surface.Uid.t Imandra_surface.Hints.t ->
  ?name:t ->
  unit ->
  unit

val all : unit -> (id * goal) list

(** {2 Report} *)

val imandra_custom_css : string
(** Builtin custom CSS for Imandra *)

val report : ?custom_css:string -> ?compressed:bool -> string -> unit
(** Write report to given filename *)
