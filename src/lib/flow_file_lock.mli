
open Core.Std
open Flow_base

val lock :
  string ->
  (bool,
   [> `lock of
       [> `path of string ] *
         [> `unix_link of exn | `write_file of exn ] ])
    Flow_base.t

val unlock :
  string ->
  (unit, [> `lock of [> `path of string ] * [> `unix_unlink of exn ] ])
    Flow_base.t

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

val do_with_locks :
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
