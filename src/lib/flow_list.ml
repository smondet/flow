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

