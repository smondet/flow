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

open Core.Std
open Flow_base

(** Returns the list of results if all succeed, or the first error. *)
let while_sequential:
    'a list -> f:('a -> ('c, 'b) t) -> ('c list, 'b) t
  = fun (type b) (l: 'a list) ~(f: 'a -> ('c, b) t) ->
  let module Map_sequential = struct
    exception Local_exception of b
    let ms l f =
      bind_on_error
        (catch_deferred
           (fun () ->
             Lwt_list.map_s (fun o ->
               Lwt.bind (f o) (function
               | Ok oo -> Lwt.return oo
               | Error ee -> Lwt.fail (Local_exception ee))) l))
        (function Local_exception e -> error e
        | e ->
          failwithf "Expecting only Local_exception, but got: %s"
            (Exn.to_string e) ())
  end in
  Map_sequential.ms l f

let for_sequential:
    'a list -> f:('a -> ('c, 'b) t) -> ('c list * 'b list, 'd) t
  = fun l ~f ->
    let open Lwt in
    Lwt_list.map_s (fun elt -> f elt) l
    >>= fun results ->
    return (Ok (List.partition_map results
                  (function Ok x -> `Fst x | Error e -> `Snd e)))

let for_concurrent:
    'a list -> f:('a -> ('c, 'b) t) -> ('c list * 'b list, 'd) t
  = fun l ~f ->
    let open Lwt in
    Lwt_list.map_p (fun elt -> f elt) l
    >>= fun results ->
    return (Ok (List.partition_map results
                  (function Ok x -> `Fst x | Error e -> `Snd e)))


let for_concurrent_with_index l ~f =
  let with_indexes = List.mapi l ~f:(fun i a -> (i, a)) in
  for_concurrent with_indexes ~f:(fun (i, a) -> f i a)

