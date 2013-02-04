(** Basic system access functions. *)

open Core.Std
open Flow_base


(** Block for a given amount of seconds ([Lwt_unix.sleep]). *)
val sleep: float -> (unit, [> `io_exn of exn ]) t    

  
(** Make [/bin/sh] execute a command. *)
val system_command: string ->
  (unit,
   [> `system_command_error of string *
       [> `exited of int | `exn of exn | `signaled of int | `stopped of int ]
   ]) t

(** Execute a shell command and return its [stdout, stderr]. *)
val get_system_command_output: string ->
  (string * string,
   [> `system_command_error of string *
       [> `exited of int | `exn of exn | `signaled of int | `stopped of int ]
   ]) t

val with_timeout : float ->
  f:(unit -> ('a, [> `io_exn of exn | `timeout of float ] as 'b) t) ->
  ('a, 'b) t
(** Execute a function [f] with a timeout (in seconds). If [f] throws
    an exception it will be passed as [`io_exn e], if the functions
    timeouts the error will be [`timeout time]. *)
