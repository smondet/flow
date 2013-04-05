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
open Flow

let say fmt =
  ksprintf (fun s -> eprintf "%s\n%!" s) fmt

let wrap_deferred_io f =
  wrap_deferred (fun () -> f ()) ~on_exn:(fun e -> `io_test_exn e)

let fail_test fmt =
  ksprintf (fun s -> error (`failed_test s)) fmt

let main () =
  let tmp_dir = Filename.temp_dir ~in_dir:"/tmp" "flow_file_lock_test" "" in
  System.remove tmp_dir
  >>= fun () ->
  System.ensure_directory_path tmp_dir
  >>= fun () ->

  let inexistent = Filename.concat tmp_dir "inexistent" in
  File_lock.lock inexistent
  >>= begin function
  | true -> return ()
  | false -> fail_test "inexistent could not be locked (%s)" inexistent
  end
  >>= fun () ->

  File_lock.lock inexistent
  >>= begin function
  | false -> return ()
  | true -> fail_test "inexistent could be locked twice (%s)" inexistent
  end
  >>= fun () ->

  File_lock.unlock inexistent
  >>= fun () ->
  File_lock.lock inexistent
  >>= begin function
  | true -> return ()
  | false -> fail_test "inexistent could not be locked after unlock (%s)" inexistent
  end
  >>= fun () ->

  File_lock.unlock inexistent
  >>= fun () ->

  (* After unlocking the directory should be empty again. *)
  System.file_tree tmp_dir
  >>= begin function
  | `node (d, []) when d = Filename.basename tmp_dir -> return ()
  | f -> error (`test_unexpected_file_tree f)
  end
  >>= fun () ->

  let to_be_created = Filename.concat tmp_dir "to_be_created" in
  File_lock.lock to_be_created
  >>= begin function
  | true -> return ()
  | false -> fail_test "to_be_created could not be locked (%s)" to_be_created
  end
  >>= fun () ->

  IO.write_file to_be_created ~content:"some content"
  >>= fun () ->

  File_lock.lock to_be_created
  >>= begin function
  | false -> return ()
  | true -> fail_test "to_be_created could be locked twice (%s)" to_be_created
  end
  >>= fun () ->

  File_lock.unlock to_be_created
  >>= fun () ->
  File_lock.lock to_be_created
  >>= begin function
  | true -> return ()
  | false -> fail_test "to_be_created could not be locked after unlock (%s)" to_be_created
  end
  >>= fun () ->

  File_lock.unlock to_be_created
  >>= fun () ->

  (* After unlocking the directory should only contain 'to_be_created'. *)
  System.file_tree tmp_dir
  >>= begin function
  | `node (d, [`leaf (filename, `regular_file _)])
      when d = Filename.basename tmp_dir
      && filename = Filename.basename to_be_created -> return ()
  | f -> error (`test_unexpected_file_tree f)
  end
  >>= fun () ->


  File_lock.with_lock_gen to_be_created (fun () ->
    return ()
  )
  >>= begin function
  | `ok () -> return ()
  | other -> error (`test_unexpected_with_lock_gen other)
  end
  >>= fun () ->

  File_lock.with_lock_gen to_be_created (fun () ->
    error (`string "voluntary error")
  )
  >>= begin function
  | `error (`string "voluntary error") -> return ()
  | other -> error (`test_unexpected_with_lock_gen other)
  end
  >>= fun () ->

  File_lock.with_lock_gen to_be_created (fun () ->
    while_sequential (List.init 42 (fun _ -> ())) (fun () ->
      File_lock.lock to_be_created
      >>= begin function
      | false -> return ()
      | true ->
        fail_test "to_be_created could be locked inside with_lock_gen (%s)"
          to_be_created
      end)
    >>= fun (_ : unit list) ->
    return ()
  )
  >>= begin function
  | `ok () -> return ()
  | `error (`fail_test s) -> fail_test "→ %s" s
  | other -> error (`test_unexpected_with_lock_gen other)
  end
  >>= fun () ->

  File_lock.with_locks_gen [to_be_created; inexistent] (fun () ->
    return ()
  )
  >>= begin function
  | `ok () -> return ()
  | other -> error (`test_unexpected_with_locks_gen other)
  end
  >>= fun () ->

  File_lock.with_locks_gen [to_be_created; inexistent] (fun () ->
    error (`string "voluntary error")
  )
  >>= begin function
  | `error (`string "voluntary error") -> return ()
  | other -> error (`test_unexpected_with_locks_gen other)
  end
  >>= fun () ->

  File_lock.with_locks_gen [to_be_created; inexistent] (fun () ->
    File_lock.lock to_be_created
    >>= begin function
    | false -> return ()
    | true ->
      fail_test "to_be_created could be locked inside with_locks_gen (%s)"
        to_be_created
    end
    >>= fun () ->
    File_lock.lock inexistent
    >>= begin function
    | false -> return ()
    | true ->
      fail_test "inexistent could be locked inside with_locks_gen (%s)"
        inexistent
    end
  )
  >>= begin function
  | `ok () -> return ()
  | other -> error (`test_unexpected_with_locks_gen other)
  end
  >>= fun () ->


  File_lock.lock to_be_created
  >>= begin function
  | true -> return ()
  | false ->
    fail_test "to_be_created could not be locked after with_locks_gen (%s)"
      to_be_created
  end
  >>= fun () ->
  File_lock.with_locks_gen ~wait:0.1 ~retry:4
    [to_be_created; inexistent] ~f:(fun () ->
    say  "THIS SHOULD NEVER BE PRINTED";
    error (`string "THIS SHOULD NEVER BE PRINTED")
  )
  >>< begin function
  | Ok _ -> fail_test "with_locks_gen did not fail on locked file"
  | Error (`lock (`paths _, `too_many_retries (0.1, 4))) -> return ()
  | Error e -> error (`test_unexpected_with_locks_gen e)
  end
  >>= fun () ->
  File_lock.unlock to_be_created
  >>= fun () ->

  let who_has_the_lock = ref None in
  let i_got_the_lock s =
    match !who_has_the_lock with
    | None ->
      who_has_the_lock := Some s;
      return ()
    | Some other -> fail_test "%s got the lock but %s already had it" s other
  in
  let i_am_going_to_release_the_lock s =
    match !who_has_the_lock with
    | Some m when m = s -> who_has_the_lock := None; return ()
    | None -> fail_test "%s is releasing a lock they don't have !" s
    | Some other -> fail_test "%s is releasing a lock owned by %s" s other
  in
  for_concurrent (List.init 20 (fun i ->
      match Random.int 3 with
      | 0 -> `locker i
      | 1 -> `with_locks i
      | _ -> `error_in_lock_gen i))
    begin function
    | `locker i ->
      let name = sprintf "locker %d" i in
      File_lock.lock to_be_created
      >>= begin function
      | true ->
        i_got_the_lock name >>= fun () ->
        System.sleep (Random.float 0.2) >>= fun () ->
        i_am_going_to_release_the_lock name >>= fun () ->
        File_lock.unlock to_be_created
      | false ->
        return ()
      end
    | `with_locks i ->
      let name = sprintf "with_locks %d" i in
      let a1 = Filename.concat tmp_dir "inexistent1" in
      let a2 = Filename.concat tmp_dir "inexistent2" in
      let a3 = Filename.concat tmp_dir "inexistent3" in
      File_lock.with_locks
        ~wait:0.2 ~retry:10 [a1; to_be_created; a2; a3] ~f:(fun () ->
        i_got_the_lock name >>= fun () ->
        System.sleep (Random.float 0.2) >>= fun () ->
        i_am_going_to_release_the_lock name >>= fun () ->
        return ())
      >>< begin function
      | Ok () -> return ()
      | Error (`lock (`paths _, `too_many_retries _)) ->
        (* This is a legitimate error. *)
        return ()
      | Error e -> error e
      end
    | `error_in_lock_gen i ->
      let name = sprintf "error_in_lock_gen %d" i in
      File_lock.with_lock_gen
        ~wait:0.3 ~retry:10 to_be_created ~f:(fun () ->
        i_got_the_lock name >>= fun () ->
        System.sleep (Random.float 0.2) >>= fun () ->
        i_am_going_to_release_the_lock name >>= fun () ->
        error (`string "voluntary error")
      )
      >>< begin function
      | Ok (`error (`string "voluntary error")) -> return ()
      | Error (`lock (`path _, `too_many_retries _)) ->
        (* This is a legitimate error. *)
        return ()
      | Error  e -> error e
      | Ok other -> error (`test_unexpected_with_lock_gen other)
      end
    end
  >>= fun ((_ : unit list), errors) ->
  begin match errors with
  | [] -> return ()
  | l -> fail_test "1st concurrent test failed: [\n    %s\n]"
           (List.map l (fun e ->
              <:sexp_of<
         [> `failed_test of string
                | `error of [> `string of string ]
          | `lock of
              [> `path of string | `paths of string list ] *
              [> `system_sleep of exn
                | `multiple of
                   [> `lock of
                        string * [> `unix_link of exn | `write_file of exn ]
                    | `unlock of string * exn ]
                   Core.Std.List.t
               | `too_many_retries of float * int
               | `unix_link of exn
               | `unix_unlink of exn
               | `write_file of exn ]
          | `system_exn of exn
          | `multiple of [> `unlock of string * exn ] list
          | `test_unexpected_with_lock_gen of
              [> `error of
                   [> `failed_test of string
                    | `string of string
                    | `system_exn of exn ]
               | `error_and_not_unlocked of
                   [> `failed_test of string
                    | `string of string
                    | `system_exn of exn ] *
                   [> `lock of
                        [> `path of string ] * [> `unix_unlink of exn ] ]
               | `ok of unit
               | `ok_but_not_unlocked of
                   unit *
                   [> `lock of
                        [> `path of string ] * [> `unix_unlink of exn ] ] ] ]
              >> e |! Sexp.to_string_hum) |! String.concat ~sep:"\n    ")
  end
  >>= fun () ->

  (* After the do_with_* the directory should only contain 'to_be_created'. *)
  System.file_tree tmp_dir
  >>= begin function
  | `node (d, [`leaf (filename, `regular_file _)])
      when d = Filename.basename tmp_dir
      && filename = Filename.basename to_be_created -> return ()
  | f -> error (`test_unexpected_file_tree f)
  end
  >>= fun () ->





  return ()

let () =
  match Lwt_main.run (main ()) with
  | Ok () -> eprintf "End: OK.\n%!"
  | Error e ->
    eprintf "End with Error:\n%s\n%!"
      (<:sexp_of<

          [> `failed_test of string
          | `lock of
              [> `path of string | `paths of string list ] *
                [> `multiple of
                    [> `lock of
                        string * [> `unix_link of exn | `write_file of exn ]
                    | `unlock of string * exn ]
                      Core.Std.List.t
                | `system_sleep of exn
                | `too_many_retries of float * int
                | `unix_link of exn
                | `unix_unlink of exn
                | `write_file of exn ]
          | `system of
              [> `file_info of string
              | `file_tree of string
              | `list_directory of string
              | `make_directory of string
              | `remove of string ] *
                [> `exn of exn
                | `file_not_found of string
                | `wrong_access_rights of int ]
          | `test_unexpected_with_lock_gen of
              [> `error of
                  [> `fail_test of string
                  | `failed_test of string
                  | `lock of
                      [> `path of string ] *
                        [> `unix_link of exn | `write_file of exn ]
                  | `string of string ]
              | `error_and_not_unlocked of
                  [> `fail_test of string
                  | `failed_test of string
                  | `lock of
                      [> `path of string ] *
                        [> `unix_link of exn | `write_file of exn ]
                  | `string of string ]
                *
                    [> `lock of
                        [> `path of string ] * [> `unix_unlink of exn ] ]
              | `ok of unit
              | `ok_but_not_unlocked of
                  unit *
                    [ `lock of
                        [ `path of string ] * [ `unix_unlink of Core.Exn.t ] ] ]
          | `test_unexpected_with_locks_gen of
              [> `error of
                   [> `failed_test of string
                    | `lock of
                        [> `path of string ] *
                        [> `unix_link of exn | `write_file of exn ]
                    | `string of string ]
               | `error_and_not_unlocked of
                   [> `failed_test of string
                    | `lock of
                        [> `path of string ] *
                        [> `unix_link of exn | `write_file of exn ]
                    | `string of string ] * [> `multiple of [> `unlock of string * exn ] list ]
               | `lock of
                   [> `paths of string list ] *
                   [> `multiple of
                        [> `lock of
                             string *
                             [> `unix_link of exn | `write_file of exn ]
                         | `unlock of string * exn ]
                        Core.Std.List.t
                    | `system_sleep of exn
                    | `too_many_retries of float * int ]
               | `ok of unit
               | `ok_but_not_unlocked of
                   unit *
                   [ `multiple of [ `unlock of string * Core.Exn.t ] list ] ]

          | `test_unexpected_file_tree of Flow.System.file_tree
          | `write_file_error of string * Core.Exn.t ]
       >> e
       |! Sexp.to_string_hum)

