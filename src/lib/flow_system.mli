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

(** Quick information on files. *)
type file_info =
[ `absent
| `file of int
| `symlink of string
| `block_device
| `character_device
| `directory
| `fifo
| `socket]

val sexp_of_file_info: file_info -> Sexp.t
val file_info_of_sexp: Sexp.t -> file_info

(** Get information about a path (whether it exists, its size, or
    sym-link destination). If [follow_symlink] is [false]
    (default) use [lstat] (so the result can be [`symlink _]), if [true]
    call [stat] (information about the target).  *)
val file_info :
  ?follow_symlink:bool -> string ->
  (file_info,
   [> `system of [> `file_info of string ] * [> `exn of exn ] ]) t

(** Get all the children of a directory, through a [next] stream-like
    function. *)
val list_directory: string ->
  [ `stream of
      (unit ->
       (string option,
        [> `system of [> `list_directory of string ] * [> `exn of exn ] ]) t) ]

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

(** Specification of a “destination” for [copy] *)
type copy_destination = [
| `into_directory of string
| `as_new of string
]

(** Copy files or directories (recursively).

    If [ignore_strange] is true [copy] won't fail on block/character
    devices, fifos or sockets (defaults to [false]).

    The [buffer_size] (default [64_000]) is used both for reading and
    writing files.

    On can [`fail] on symbolic links, [`follow] them, or [`redo] a new
    symlink with the same target.

*)
val copy:
  ?ignore_strange:bool ->
  ?symlinks:[ `fail | `follow | `redo ] ->
  ?buffer_size:int ->
  src:string -> copy_destination ->
  (unit,
   [> `system of
       [> `copy of string
       | `file_info of string
       | `list_directory of string
       | `make_symlink of string * string
       | `mkdir of string ] *
         [> `already_exists
         | `exn of exn
         | `file_not_found of string
         | `wrong_access_rights of int
         | `wrong_file_kind of
             string *
               [> `block_device | `character_device
               | `fifo | `socket | `symlink of string ] ] ]) t

(** Try to move [src] to [dest] using [Lwt_unix.rename], if it works,
    return [`moved] if it does not work but [copy] could work
    (i.e. both paths are not in the same {i device}) return
    [`must_copy]. *)
val move_in_same_device: src:string -> copy_destination ->
  ([ `moved | `must_copy ],
   [> `system of [> `move of string ] * [> `exn of exn ] ]) t

(** Heavy-weight function trying to mimic the behavior the UNIX command “mv”
    (c.f. {{:http://www.openbsd.org/cgi-bin/cvsweb/src/bin/mv/mv.c?rev=1.35;content-type=text%2Fplain;only_with_tag=HEAD}mv.c}):
    it tries [move_in_same_device] and if it returns [`must_copy] it
    calls [copy] and [remove] (if [copy] fails, the [remove] won't
    happen but there will be no clean-up of the files already
    copied).
*)
val move:
  ?ignore_strange:bool ->
  ?symlinks:[ `fail | `follow | `redo ] ->
  ?buffer_size:int ->
  src:string -> copy_destination ->
  (unit,
   [> `system of
       [> `copy of string
       | `move of string
       | `remove of string
       | `file_info of string
       | `list_directory of string
       | `make_symlink of string * string
       | `mkdir of string ] *
         [> `already_exists
         | `exn of exn
         | `file_not_found of string
         | `wrong_access_rights of int
         | `wrong_file_kind of
             string *
               [> `block_device | `character_device
               | `fifo | `socket | `symlink of string ] ] ]) t

(** Representation of a hierarchy of files ([`leaf]) and directories ([`node]). *)
type file_tree = [
| `node of string * file_tree list
| `leaf of string * file_info
] with sexp

(** Obtain the [file_tree] starting at a given path. *)
val file_tree :
  ?follow_symlinks:bool ->
  string ->
  (file_tree, [> `system of
                 [> `file_info of string
                 | `file_tree of string
                 | `list_directory of string ] *
                   [> `exn of exn
                   | `file_not_found of string
                   | `wrong_file_kind of 'b ] ]) t
