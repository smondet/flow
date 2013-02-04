(** Monadic “loops” on lists. *)

open Core.Std
open Flow_base

(** Sequentially launch [f] on the first argument and
    get out of the loop at the first error.
    The function returns the list of results if all succeed, or the
    first error.
*)
val while_sequential: 'a list -> f:('a -> ('c, 'b) t) -> ('c list, 'b) t

(** Sequentially launch [f] on the first argument and process the
    whole list even if there are errors.
    The function returns the list of successes and the list of errors.  *)
val for_sequential: 'a list -> f:('a -> ('c, 'b) t) -> ('c list * 'b list, 'd) t

(** Like [for_sequential] but all the threads are launched concurrently. *) 
val for_concurrent: 'a list -> f:('a -> ('c, 'b) t) -> ('c list * 'b list, 'd) t

(** Like [for_concurrent] but with the index in the list passed to the
    function. *)
val for_concurrent_with_index:
  'a list -> f:(int -> 'a -> ('c, 'b) t) -> ('c list * 'b list, 'd) t
