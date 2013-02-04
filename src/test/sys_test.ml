open Core.Std
open Flow

let say fmt =
  ksprintf (fun s -> eprintf "%s\n%!" s) fmt

let test_mkdir () =
  let tmp = Filename.temp_dir "sys_test_mkdir" "_dir" in
  ksprintf Sys.system_command "rm -fr %s" tmp
  >>= fun () ->
  Sys.mkdir ~perm:0o777 tmp
  >>= fun () ->
  begin
    Sys.mkdir "/please_dont_run_tests_as_root"
    >>< begin function
    | Ok () -> say "ERROR: This should have failed!!"; return ()
    | Error (`system (`mkdir _, `wrong_access_rights _)) -> return ()
    | Error e -> say "ERROR: Got wrong error"; error e
    end
  end
  >>= fun () ->
  begin
    Sys.mkdir tmp
    >>< begin function
    | Ok () -> say "ERROR: This should have failed!!"; return ()
    | Error (`system (`mkdir _, `already_exists)) -> return ()
    | Error e -> say "ERROR: Got wrong error"; error e
    end
  end
  >>= fun () ->
  let path = Filename.concat tmp "some/very/long/path" in
  Sys.mkdir_p path
  >>= fun () ->
  ksprintf Sys.system_command "find %s -type d" tmp
  >>= fun () ->
  say "test_mkdir: OK";
  return ()

let test_file_info () =
  let tmp = Filename.temp_dir "sys_test_file_info" "_dir" in
  ksprintf Sys.system_command "cd %s && ln -s /tmp symlink_to_dir" tmp
  >>= fun () ->
  ksprintf Sys.system_command "cd %s && ln -s /etc/passwd symlink_to_file" tmp
  >>= fun () ->
  let check ?follow_symlink matches path =
    Sys.file_info ?follow_symlink path
    >>= begin function
    | o when matches o -> return ()
    | e -> error (`wrong_file_info (path, e))
    end
  in
  check ((=) `directory) "/" >>= fun () ->
  check (function `file _ -> true | _ -> false) "/etc/passwd" >>= fun () ->
  ksprintf (check ((=) (`symlink "/etc/passwd"))) "%s/symlink_to_file" tmp
  >>= fun () ->
  ksprintf (check ((=) (`symlink "/tmp"))) "%s/symlink_to_dir" tmp
  >>= fun () ->
  check ((=) `absent) "/sldkfjslakjfdlksj"
  >>= fun () ->

  ksprintf Sys.system_command "ls -l %s " tmp
  >>= fun () ->
  say "test_file_info: OK";
  return ()


let main () =
  say "sys_test: GO!";
  test_mkdir ()
  >>= fun () ->
  test_file_info ()

let () =
  match Lwt_main.run (main ()) with
  | Ok () -> ()
  | Error e ->
    eprintf "End with Error:\n%s\n%!"
      (<:sexp_of<
          [ `system of
              [`file_info of string | `mkdir of string ] *
                [ `already_exists | `exn of exn | `wrong_access_rights of int ]
          | `wrong_file_info of string *
              [ `absent
              | `block_device
              | `character_device
              | `directory
              | `fifo
              | `file of int
              | `socket
              | `symlink of string ]
          | `system_command_error of
              string *
                [ `exited of int
                | `exn of exn
                | `signaled of int
                | `stopped of int ]
          ]  >> e
       |! Sexp.to_string_hum)
