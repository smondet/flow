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

include  Monad.Make2(struct
  type ('a, 'b) t = ('a, 'b) Result.t Lwt.t

  let return x = Lwt.return (Ok x)
  let bind x f =
    Lwt.bind x (function
    | Error e -> Lwt.return (Error e)
    | Ok o -> f o)

  let map = `Custom (fun x ~f ->
    Lwt.map (function
    | Error _ as e -> e
    | Ok o -> Ok (f o)
    ) x
    )

end)
type ('a, 'b) t = ('a, 'b) Result.t Lwt.t

let error e = Lwt.return (Error e)

let (>><) m f = Lwt.bind m f

let bind_on_error m ~f = Lwt.bind m (function
  | Ok o -> Lwt.return (Ok o)
  | Error e -> (f e))

let double_bind m ~ok ~error =
  Lwt.bind m (function
  | Ok o -> ok o
  | Error e -> error e)

let catch_deferred f =
  Lwt.catch
    (fun () ->
      let a_exn_m : 'a Lwt.t = f () in
      Lwt.bind a_exn_m (fun x -> Lwt.return (Ok x)))
    (fun e -> Lwt.return (Error e))

let wrap_deferred ~on_exn f =
  let caught = catch_deferred f in
  double_bind caught
    ~ok:return
    ~error:(fun exn -> error (on_exn exn))

let wrap ~on_exn f =
  let caught = catch_deferred (fun () -> Lwt.return (f ())) in
  double_bind caught
    ~ok:return
    ~error:(fun exn -> error (on_exn exn))

let map_option: 'a option -> f:('a -> ('b, 'error) t) -> ('b option, 'error) t
  = fun o ~f ->
    begin match o with
    | None -> return None
    | Some s ->
      f s
      >>= fun g ->
      return (Some g)
    end

let of_result r = Lwt.return r

