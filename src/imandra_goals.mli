
(** {1 Goal Manager} *)

type t = {
  name     : string;
  section  : string option;
  desc     : string;
  owner    : owner option;
  status   : status;
  expected : expected;
  idx      : int;
  hints    : Imandra_surface.Uid.t Imandra_surface.Hints.t option;
  upto     : Imandra_syntax.Logic_ast.upto option
}

and status =
  | Open of { assigned_to : owner option }
  | Closed of {
    timestamp : float;
    duration : float;
    result : Verify.t;
  }
  | Error of string

and expected = True | False | Unknown

and owner = string

and id = string * string option (* name, section *)

type goal = t

(** Start a new goal *)
val init :
  ?section:string ->
  ?owner:owner ->
  ?expected:expected ->
  ?hints:Imandra_surface.Uid.t Imandra_surface.Hints.t ->
  ?upto:Imandra_syntax.Logic_ast.upto ->
  desc:string -> name:string ->
  unit -> unit

val close_goal :
  ?hints:Imandra_surface.Uid.t Imandra_surface.Hints.t ->
  t -> t

val close :
  ?hints:Imandra_surface.Uid.t Imandra_surface.Hints.t ->
  ?name:t -> unit -> unit

val verify_ :
  ?hints:Imandra_surface.Uid.t Imandra_surface.Hints.t ->
  ?name:t -> unit -> unit

val all : unit -> (id * goal) list

(** {2 Report} *)

(** Builtin custom CSS for Imandra *)
val imandra_custom_css : string

(** Write report to given filename *)
val report :
  ?custom_css:string ->
  ?compressed:bool -> string -> unit
