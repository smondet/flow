(** Basic system access functions. *)

open Core.Std
open Flow_base

(** Write a string to a file. *)
val write_file: string -> content:string ->
  (unit, [> `write_file_error of (string * exn)]) t

(** Read a string from a file. *)
val read_file: string -> 
  (string, [> `read_file_error of (string * exn)]) t
    

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

