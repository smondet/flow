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


let main () =
  say "sys_test: GO!";
  test_mkdir ()
  >>= fun () ->
  return ()
    
let () =
  match Lwt_main.run (main ()) with
  | Ok () -> ()
  | Error e ->
    eprintf "End with Error:\n%s\n%!"
      (<:sexp_of<
          [ `system of
              [ `mkdir of string ] *
                [ `already_exists | `exn of exn | `wrong_access_rights of int ]
          | `system_command_error of
              string *
                [ `exited of int
                | `exn of exn
                | `signaled of int
                | `stopped of int ]
          ]  >> e
       |! Sexp.to_string_hum)
