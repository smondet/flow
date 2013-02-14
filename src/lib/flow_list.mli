(**************************************************************************)
(*  Copyright (c) 2012, 2013,                                             *)
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
