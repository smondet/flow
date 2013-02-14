(** Basic system access functions. *)

open Core.Std
open Flow_base


(** Block for a given amount of seconds ([Lwt_unix.sleep]). *)
val sleep: float -> (unit, [> `system_exn of exn ]) t

(** Manipulate [/bin/sh] commands (flavors of [Unix.system]).  *)
module Shell : sig

  (** Make [/bin/sh] execute a command, fail if it does not return 0. *)
  val do_or_fail: string ->
    (unit,
     [> `shell of string *
         [> `exited of int | `exn of exn | `signaled of Signal.t | `stopped of int ]
     ]) t

  (** Execute a shell command and return its standard output, standard error,
      and exit code
      [stdout, stderr]. *)
  val execute:
    string ->
    (string * string * [ `exited of int | `signaled of Signal.t | `stopped of int ],
     [> `shell of string * [> `exn of exn ]]) t
end

(** Execute a function [f] with a timeout (in seconds). If [f] throws
    an exception it will be passed as [`system_exn e], if the functions
    timeouts the error will be [`timeout time]. *)
val with_timeout : float ->
  f:(unit -> ('a, [> `system_exn of exn | `timeout of float ] as 'b) t) ->
  ('a, 'b) t

(** Create a new empty directory or fail if it already exists
   (i.e. like [mkdir]). The default permissions are [0o700]. *)
val make_new_directory: ?perm:int -> string ->
  (unit,
   [> `system of
       [> `make_directory of string ] *
         [> `already_exists | `exn of exn | `wrong_access_rights of int ] ]) t

(** Create as many directories as needed (can be 0) to ensure that the
    directory path exists (like [mkdir -p]). The default permissions
    are [0o700].  *)
val ensure_directory_path: ?perm:int -> string ->
  (unit,
   [> `system of
       [> `make_directory of string ] *
         [> `exn of exn | `wrong_access_rights of int ] ]) t

(** Quick information on files. *)
type file_info =
[ `absent
| `regular_file of int
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

(** Remove a file or a directory recursively. [remove] does not fail
    if the file does not exist.  *)
val remove: string ->
  (unit,
   [> `system of [> `file_info of string
                 | `remove of string
                 | `list_directory of string ] * [> `exn of exn ] ]) t

(** Make a symbolic link [link_path] pointing at
    [target]. [make_symlink] fails if the file [link_path] already
    exists. *)
val make_symlink: target:string -> link_path:string ->
  (unit,
   [> `system of [> `make_symlink of string * string]
     * [> `file_exists of string
       | `exn of exn ] ]) t

(** Specification of a “destination” for [copy] and [move]. *)
type file_destination = [
| `into of string (** [`into path] means copy 'file' into the {b directory} path. *)
| `onto of string (** [`onto path] means copy 'file' {b as} [path]. *)
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
  ?if_exists:[ `fail | `overwrite | `update ] ->
  src:string -> file_destination ->
  (unit,
   [> `system of
       [> `copy of string
       | `file_info of string
       | `list_directory of string
       | `make_symlink of string * string
       | `remove of string
       | `make_directory of string ] *
         [> `already_exists
         | `exn of exn
         | `file_exists of string
         | `wrong_path of string
         | `file_not_found of string
         | `wrong_access_rights of int
         | `not_a_directory of string
         | `wrong_file_kind of
             string *
               [> `block_device | `character_device
               | `fifo | `socket | `symlink of string ] ] ]) t

(** Try to move [src] to [dest] using [Lwt_unix.rename], if it works,
    return [`moved] if it does not work but [copy] could work
    (i.e. both paths are not in the same {i device}) return
    [`must_copy]. *)
val move_in_same_device:
  ?if_exists:[ `fail | `overwrite | `update ] ->
  src:string -> file_destination ->
  ([ `moved | `must_copy ],
   [> `system of [> `move of string | `file_info of string ]
     * [> `exn of exn
       | `file_exists of string] ]) t

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
  ?if_exists:[ `fail | `overwrite | `update ] ->
  src:string -> file_destination ->
  (unit,
   [> `system of
       [> `copy of string
       | `move of string
       | `remove of string
       | `file_info of string
       | `list_directory of string
       | `make_symlink of string * string
       | `make_directory of string ] *
         [> `already_exists
         | `exn of exn
         | `file_exists of string
         | `wrong_path of string
         | `file_not_found of string
         | `wrong_access_rights of int
         | `not_a_directory of string
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
