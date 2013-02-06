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

(** Create a directory (default permissions: [0o700]). *)
val mkdir :
  ?perm:int ->
  string ->
  (unit,
   [> `system of
       [> `mkdir of string ] *
         [> `already_exists
         | `exn of exn
         | `wrong_access_rights of int ] ]) t

(** Create a directory and its potential parents (i.e. like [mkdir -p]).  *)
val mkdir_p : ?perm:int -> string ->
  (unit,
   [> `system of
       [> `mkdir of string ] *
         [> `exn of exn | `wrong_access_rights of int ] ]) t

(** Get information about a path (whether it exists, its size, or
    sym-link destination). If [follow_symlink] is [false]
    (default) use [lstat] (so the result can be [`symlink _]), if [true]
    call [stat] (information about the target).  *)
val file_info :
  ?follow_symlink:bool -> string ->
  ([ `absent
   | `file of int
   | `symlink of string
   | `block_device
   | `character_device
   | `directory
   | `fifo
   | `socket],
   [> `system of [> `file_info of string ] * [> `exn of exn ] ]) t

(** Get all the children of a directory, through a [next] stream-like
    function. *)
val list_directory: string ->
  (unit ->
   (string option,
    [> `system of [> `list_directory of string ] * [> `exn of exn ] ]) t)

(** Remove a file or a directory recursively. *)
val remove: string ->
  (unit,
   [> `system of [> `file_info of string
                 | `remove of string
                 | `list_directory of string ] * [> `exn of exn ] ]) t

(** Make a symbolic link [link_path] pointing at [target]. *)
val make_symlink: target:string -> link_path:string ->
  (unit,
   [> `system of [> `make_symlink of string * string] * [> `exn of exn ] ]) t
