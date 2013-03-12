(**************************************************************************)
(*  Copyright (c) 2013,                                                   *)
(*                           Sebastien Mondet <seb@mondet.org>,           *)
(*                           Ashish Agarwal <agarwal1975@gmail.com>.      *)
(*                                                                        *)
(*  Permission to use, copy, modify, and/or distribute this software for  *)
(*  any purpose with or without fee is hereby granted, provided that the  *)
(*  above  copyright notice  and this  permission notice  appear  in all  *)
(*  copies.                                                               *)
(*                                                                        *)
(*  THE  SOFTWARE IS  PROVIDED  "AS  IS" AND  THE  AUTHOR DISCLAIMS  ALL  *)
(*  WARRANTIES  WITH  REGARD  TO  THIS SOFTWARE  INCLUDING  ALL  IMPLIED  *)
(*  WARRANTIES  OF MERCHANTABILITY AND  FITNESS. IN  NO EVENT  SHALL THE  *)
(*  AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL  *)
(*  DAMAGES OR ANY  DAMAGES WHATSOEVER RESULTING FROM LOSS  OF USE, DATA  *)
(*  OR PROFITS,  WHETHER IN AN  ACTION OF CONTRACT, NEGLIGENCE  OR OTHER  *)
(*  TORTIOUS ACTION,  ARISING OUT  OF OR IN  CONNECTION WITH THE  USE OR  *)
(*  PERFORMANCE OF THIS SOFTWARE.                                         *)
(**************************************************************************)

(** An attempt at NFS-compliant file-locking. *)

open Core.Std
open Flow_base

val lock :
  string ->
  (bool,
   [> `lock of
       [> `path of string ] *
         [> `unix_link of exn | `write_file of exn ] ])
    Flow_base.t
(** Try to lock a file, return [true] if succeed.  *)

val unlock :
  string ->
  (unit, [> `lock of [> `path of string ] * [> `unix_unlink of exn ] ])
    Flow_base.t
(** Unlock a file which is locked. *)

val with_lock_gen : ?wait:float -> ?retry:int -> string ->
  f:(unit -> ('user_ok, 'user_error) Flow_base.t) ->
  ([>
   | `ok of 'user_ok
   | `ok_but_not_unlocked of
       'user_ok * [> `lock of [> `path of string ] * [> `unix_unlink of exn ] ]
   | `error of 'user_error
   | `error_and_not_unlocked of
       'user_error * [> `lock of [> `path of string ] * [> `unix_unlink of exn ] ]
   ],
   [> `lock of
       [> `path of string ] *
         [> `system_sleep of exn
         | `too_many_retries of float * int
         | `unix_link of exn
         | `write_file of exn ] ])
    Flow_base.t
(** Run a function with a file locked. Retry [retry] times after
    waiting [wait] seconds between each attempt. The return value is
    as general as possible (on the [Ok _] side): {ul
      {li [`ok _] if everything is fine.}
      {li [`ok_but_not_unlocked _] if the function succeeded but
        unlocking did not.}
      {li [`error _] if the function failed but the locking and
        unlocking were fine.}
      {li [`error_and_not_unlocked _] if both the user function and the
        unlocking failed. }
    } *)

val with_lock: ?wait:float -> ?retry:int -> string ->
  f:(unit ->
     ('a,
      [> `lock of
          [> `path of string ] *
            [> `system_sleep of exn
            | `too_many_retries of float * int
            | `unix_link of exn
            | `unix_unlink of exn
            | `write_file of exn ] ]
        as 'b)
       Flow_base.t) ->
  ('a, 'b) Flow_base.t
(** Do like [with_lock_gen] but merge errors (locking errors take
    precedence over user-function results). *)

val with_locks_gen :
  ?wait:float ->
  ?retry:int ->
  string list ->
  f:(unit -> ('user_ok, 'user_error) Flow_base.t) ->
  ([> `error of 'user_error
   | `error_and_not_unlocked of
       'user_error * [> `multiple of [> `unlock of string * exn ] list ]
   | `ok of 'user_ok
   | `ok_but_not_unlocked of
       'user_ok * [> `multiple of [> `unlock of string * exn ] list ] ],
   [> `lock of
       [> `paths of string list ] *
         [> `multiple of
             [> `lock of
                 string *
                   [> `unix_link of exn | `write_file of exn ]
             | `unlock of string * exn ]
               Core.Std.List.t
         | `system_sleep of exn
         | `too_many_retries of float * int ] ])
    Flow_base.t
(** Do like [with_lock_gen] but with more than one files to lock. *)

val with_locks :
  ?wait:float ->
  ?retry:int ->
  string list ->
  f:(unit ->
     ('a,
      [> `lock of
          [> `paths of string list ] *
            [> `multiple of
                [> `lock of
                    string *
                      [> `unix_link of exn | `write_file of exn ]
                | `unlock of string * exn ]
                  Core.Std.List.t
            | `system_sleep of exn
            | `too_many_retries of float * int ]
      | `multiple of [> `unlock of string * exn ] list ]
        as 'b)
       Flow_base.t) ->
  ('a, 'b) Flow_base.t
(** Do like [with_locks_gen] but merge errors like in [with_lock]. *)

