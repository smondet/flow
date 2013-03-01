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

open Core.Std
open Flow_base
open Flow_list


let lock_path path = path ^ ".lock"
let lock_content path = path ^ ".empty"

let wrap_unix path f =
  wrap_deferred (fun () -> f ())
    ~on_exn:(fun e -> `exn e)

let do_lock path =
  Flow_io.write_file (lock_content path) ~content:(Time.(to_string (now ())))
  >>= fun () ->
  wrap_unix path (fun () ->
    let open Lwt in
    catch (fun () ->
      Lwt_unix.link (lock_content path) (lock_path path)
      >>= fun () ->
      return true)
      (fun e -> return false))

let lock path =
  do_lock path
  >>< begin function
  | Ok b -> return b
  | Error (`write_file_error (f, e)) ->
    error (`lock (`path path, `write_file e))
  | Error (`exn e) ->
    error (`lock (`path path, `unix_link e))
  end

let unlock path =
  wrap_unix path (fun () ->
    let open Lwt in
    Lwt_unix.unlink (lock_content path)
    >>= fun () ->
    Lwt_unix.unlink (lock_path path)
  )
  >>< begin function
  | Ok () -> return ()
  | Error (`exn e) -> error (`lock (`path path, `unix_unlink e))
  end

let with_lock_gen ?(wait=0.042) ?(retry=100) file ~f =
  let rec loop try_again =
    lock file
    >>= begin function
    | true ->
      f ()
      >>< begin function
      | Ok res ->
        unlock file
        >>< begin function
        | Ok () -> return (`ok res)
        | Error e -> return (`ok_but_not_unlocked (res, e))
        end
      | Error e1 ->
        unlock file
        >>< begin function
        | Ok () -> return (`error e1)
        | Error e2 -> return (`error_and_not_unlocked (e1, e2))
        end
      end
    | false when try_again > 0 ->
      Flow_system.sleep wait
      >>= fun () ->
      loop (try_again - 1)
    | false ->
      error (`too_many_retries (wait, retry))
    end
  in
  loop retry
  >>< begin function
  | Ok o -> return o
  | Error (`lock m) -> error (`lock m)
  | Error (`system_exn e) -> error (`lock (`path file, `system_sleep e))
  | Error (`too_many_retries _ as e) -> error (`lock (`path file, e))
  end

let with_lock ?wait ?retry file ~f =
  with_lock_gen ?wait ?retry file ~f
  >>= begin function
  | `ok o -> return o
  | `error e -> error e
  | `ok_but_not_unlocked (_, e) -> error e
  | `error_and_not_unlocked (_, e2) -> error (e2)
  end

let with_locks_gen ?(wait=0.042) ?(retry=100) files ~f =
  let is_locked = function `locked _ -> true | _ -> false in
  let unlock_locked l =
    for_sequential l begin function
    | `locked f -> unlock f
    | `not_locked f -> return ()
    end
    >>= begin function
    | (oks, []) -> return ()
    | (oks, errs) ->
      let cleaner =
        List.map errs (function
        | `lock (`path p, `unix_unlink e) -> `unlock (p, e)) in
      error (`multiple (cleaner : _ list))
    end
  in
  let rec loop try_again =
    for_sequential files (fun f ->
      lock f
      >>= begin function
      | true -> return (`locked f)
      | false -> return (`not_locked f)
      end)
    >>= begin function
    | (ok_list, []) when List.for_all ok_list is_locked ->
      f ()
      >>< begin function
      | Ok res ->
        unlock_locked ok_list
        >>< begin function
        | Ok () -> return (`ok res)
        | Error e -> return (`ok_but_not_unlocked (res, e))
        end
      | Error e ->
        unlock_locked ok_list
        >>< begin function
        | Ok () -> return (`error e)
        | Error e2 -> return (`error_and_not_unlocked (e, e2))
        end
      end
    | (ok_list, []) when try_again > 0 ->
      unlock_locked ok_list
      >>= fun () ->
      Flow_system.sleep wait >>= fun () ->
      loop (try_again - 1)
    | (ok_list, []) ->
      unlock_locked ok_list
      >>= fun () ->
      error (`too_many_retries (wait, retry))
    | (ok_list, errs) ->
      unlock_locked ok_list
      >>= fun () ->
      let cleaner =
        List.map errs (function
        | `lock (`path p, e) -> `lock (p, e)) in
      error (`multiple cleaner)
    end
  in
  loop retry
  >>< begin function
  | Ok o -> return o
  | Error (`lock m) -> error (`lock m)
  | Error (`multiple m) -> error (`lock (`paths files, `multiple m))
  | Error (`system_exn m) -> error (`lock (`paths files, `system_sleep m))
  | Error (`too_many_retries m) -> error (`lock (`paths files, `too_many_retries m))
  end
