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


  File_lock.do_with_lock to_be_created (fun () ->
    return ()
  )
  >>= begin function
  | `ok () -> return ()
  | other -> error (`test_unexpected_do_with_lock other)
  end
  >>= fun () ->

  File_lock.do_with_lock to_be_created (fun () ->
    error (`string "voluntary error")
  )
  >>= begin function
  | `error (`string "voluntary error") -> return ()
  | other -> error (`test_unexpected_do_with_lock other)
  end
  >>= fun () ->

  File_lock.do_with_lock to_be_created (fun () ->
    File_lock.lock to_be_created
    >>= begin function
    | false -> return ()
    | true ->
      fail_test "to_be_created could be locked inside do_with_lock (%s)"
        to_be_created
    end
  )
  >>= begin function
  | `ok () -> return ()
  | `error (`fail_test s) -> fail_test "→ %s" s
  | other -> error (`test_unexpected_do_with_lock other)
  end
  >>= fun () ->

  File_lock.do_with_locks [to_be_created; inexistent] (fun () ->
    return ()
  )
  >>= begin function
  | `ok () -> return ()
  | other -> error (`test_unexpected_do_with_locks other)
  end
  >>= fun () ->

  File_lock.do_with_locks [to_be_created; inexistent] (fun () ->
    error (`string "voluntary error")
  )
  >>= begin function
  | `error (`string "voluntary error") -> return ()
  | other -> error (`test_unexpected_do_with_locks other)
  end
  >>= fun () ->

  File_lock.do_with_locks [to_be_created; inexistent] (fun () ->
    File_lock.lock to_be_created
    >>= begin function
    | false -> return ()
    | true ->
      fail_test "to_be_created could be locked inside do_with_locks (%s)"
        to_be_created
    end
    >>= fun () ->
    File_lock.lock inexistent
    >>= begin function
    | false -> return ()
    | true ->
      fail_test "inexistent could be locked inside do_with_locks (%s)"
        inexistent
    end
  )
  >>= begin function
  | `ok () -> return ()
  | other -> error (`test_unexpected_do_with_locks other)
  end
  >>= fun () ->


  File_lock.lock to_be_created
  >>= begin function
  | true -> return ()
  | false ->
    fail_test "to_be_created could not be locked after do_with_locks (%s)"
      to_be_created
  end
  >>= fun () ->
  File_lock.do_with_locks ~wait:0.1 ~retry:4
    [to_be_created; inexistent] ~f:(fun () ->
    say  "THIS SHOULD NEVER BE PRINTED";
    error (`string "THIS SHOULD NEVER BE PRINTED")
  )
  >>< begin function
  | Ok _ -> fail_test "do_with_locks did not fail on locked file"
  | Error (`lock (`paths _, `too_many_retries (0.1, 4))) -> return ()
  | Error e -> error (`test_unexpected_do_with_locks e)
  end
  >>= fun () ->
  File_lock.unlock to_be_created
  >>= fun () ->

  for_concurrent [ `locker ; `test_with_locker ]
    begin function
    | `locker ->
      File_lock.lock to_be_created
      >>= fun _ ->
      System.sleep 0.5
      >>= fun () ->
      File_lock.unlock to_be_created
    | `test_with_locker ->
      File_lock.do_with_lock
        ~wait:0.2 ~retry:10 to_be_created ~f:(fun () ->
          error (`string "voluntary error")
        )
      >>= begin function
      | `error (`string "voluntary error") -> return ()
      | other -> error (`test_unexpected_do_with_lock other)
      end
    end
  >>= fun ((_ : unit list), errors) ->
  begin match errors with
  | [] -> return ()
  | l -> fail_test "1st concurrent test failed: [\n    %s\n]"
    (List.map l (fun e ->
      <:sexp_of<
        [> `lock of
            [> `path of string ] *
              [> `system_sleep of exn
               | `too_many_retries of float * int
               | `unix_link of exn
               | `unix_unlink of exn
               | `write_file of exn ]
        | `system_exn of exn
          | `test_unexpected_do_with_lock of
              [> `error of [> `string of string ]
               | `error_and_not_unlocked of
                   [> `string of string ] *
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
          | `test_unexpected_do_with_lock of
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
          | `test_unexpected_do_with_locks of
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

