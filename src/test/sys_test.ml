open Core.Std
open Flow

let say fmt =
  ksprintf (fun s -> eprintf "%s\n%!" s) fmt

let cmdf fmt =
  ksprintf (fun s ->
    say "CMD: %S" s;
    System.system_command s) fmt

let test_mkdir () =
  let tmp = Filename.temp_dir "sys_test_mkdir" "_dir" in
  ksprintf System.system_command "rm -fr %s" tmp
  >>= fun () ->
  System.mkdir ~perm:0o777 tmp
  >>= fun () ->
  begin
    System.mkdir "/please_dont_run_tests_as_root"
    >>< begin function
    | Ok () -> say "ERROR: This should have failed!!"; return ()
    | Error (`system (`mkdir _, `wrong_access_rights _)) -> return ()
    | Error e -> say "ERROR: Got wrong error"; error e
    end
  end
  >>= fun () ->
  begin
    System.mkdir tmp
    >>< begin function
    | Ok () -> say "ERROR: This should have failed!!"; return ()
    | Error (`system (`mkdir _, `already_exists)) -> return ()
    | Error e -> say "ERROR: Got wrong error"; error e
    end
  end
  >>= fun () ->
  let path = Filename.concat tmp "some/very/long/path" in
  System.mkdir_p path
  >>= fun () ->
  ksprintf System.system_command "find %s -type d" tmp
  >>= fun () ->
  say "test_mkdir: OK";
  return ()

let test_file_info () =
  let tmp = Filename.temp_dir "sys_test_file_info" "_dir" in
  ksprintf System.system_command "cd %s && ln -s /tmp symlink_to_dir" tmp
  >>= fun () ->
  ksprintf System.system_command "cd %s && ln -s /etc/passwd symlink_to_file" tmp
  >>= fun () ->
  let check ?follow_symlink matches path =
    System.file_info ?follow_symlink path
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

  ksprintf System.system_command "ls -l %s " tmp
  >>= fun () ->
  say "test_file_info: OK";
  return ()

let test_remove style =
  let is_present path =
    System.file_info path
    >>= begin function
    | `absent -> error (`wrong_file_info (path, `absent))
    | any -> return ()
    end in
  let is_absent path =
    System.file_info path
    >>= begin function
    | `absent -> return ()
    | e -> error (`wrong_file_info (path, e))
    end in
  let in_dir =
    match style with
    | `relative -> "test_flow_system_remove"
    | `absolute -> "/tmp/test_flow_system_remove" in
  cmdf "rm -fr %s" in_dir
  >>= fun () ->
  System.mkdir_p in_dir
  >>= fun () ->
  let test_regular = Filename.concat in_dir "reg_file" in
  IO.write_file test_regular ~content:"come content"
  >>= fun () ->
  System.file_info test_regular
  >>= begin function
  | `file l when l > 2 -> return ()
  | e -> error (`wrong_file_info (test_regular, e))
  end
  >>= fun () ->
  System.remove test_regular
  >>= fun () ->
  say "Removed %s" test_regular;
  is_absent test_regular
  >>= fun () ->
  let test_empty_dir = Filename.concat in_dir "empty_dir" in
  System.mkdir_p test_empty_dir >>= fun () ->
  is_present test_empty_dir >>= fun () ->
  System.remove test_empty_dir >>= fun () ->
  is_absent test_empty_dir >>= fun () ->
  say "Removed %s" test_empty_dir;
  let test_non_empty_dir = Filename.concat in_dir "non_empty_dir" in
  System.mkdir_p test_non_empty_dir >>= fun () ->
  let max_number_creations = ref 100 in
  let rec random_tree path =
    while_sequential (List.init (Random.int 20) ident) (fun n ->
    if !max_number_creations <= 0 then return ()
    else begin
      let name =
        ksprintf (Filename.concat path) "random%d" n in
      decr max_number_creations;
      if Random.bool ()
      then begin
        System.mkdir_p name >>= fun () ->
        random_tree name
      end else begin
        IO.write_file name ~content:"lkjsdflkjs"
      end
    end)
    >>= fun _ ->
    return ()
  in
  random_tree test_non_empty_dir
  >>= fun () ->
  cmdf "find %s | wc -l" test_non_empty_dir
  >>= fun () ->
  System.remove test_non_empty_dir >>= fun () ->
  is_absent test_non_empty_dir >>= fun () ->

  let test_symlink = Filename.concat in_dir "test_symlink" in
  wrap_io (Lwt_unix.symlink "/tmp/bouh") test_symlink
  >>= fun () ->
  is_present test_symlink >>= fun () ->
  System.remove test_symlink >>= fun () ->
  is_absent test_symlink >>= fun () ->
  say "Removed: %s" test_symlink;

  System.remove in_dir >>= fun () ->
  is_absent in_dir >>= fun () ->

  say "test_remove: OK";
  return ()


let main () =
  say "sys_test: GO!";
  test_mkdir ()
  >>= fun () ->
  test_file_info ()
  >>= fun () ->
  test_remove `relative >>= fun () ->
  test_remove `absolute >>= fun () ->
  say "sys_test: Successful End";
  return ()

let () =
  match Lwt_main.run (main ()) with
  | Ok () -> ()
  | Error e ->
    eprintf "End with Error:\n%s\n%!"
      (<:sexp_of<
          [ `system of
              [`file_info of string | `mkdir of string | `remove of string] *
                [ `already_exists | `exn of exn | `wrong_access_rights of int ]
          | `io_exn of exn
          | `write_file_error of string * exn
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
