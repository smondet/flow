open Core.Std
open Flow

let say fmt =
  ksprintf (fun s -> eprintf "%s\n%!" s) fmt

let cmdf fmt =
  ksprintf (fun s ->
    say "CMD: %S" s;
    System.Shell.do_or_fail s) fmt

let fail_test fmt =
  ksprintf (fun s -> error (`failed_test s)) fmt

let test_make_directory () =
  let tmp = Filename.temp_dir "sys_test_make_directory" "_dir" in
  ksprintf System.Shell.do_or_fail "rm -fr %s" tmp
  >>= fun () ->
  System.ensure_directory_path ~perm:0o777 tmp
  >>= fun () ->
  begin
    System.ensure_directory_path "/please_dont_run_tests_as_root"
    >>< begin function
    | Ok () -> say "ERROR: This should have failed!!"; return ()
    | Error (`system (`make_directory _, `wrong_access_rights _)) -> return ()
    | Error e -> say "ERROR: Got wrong error"; error e
    end
  end
  >>= fun () ->
  begin
    System.make_new_directory tmp
    >>< begin function
    | Ok () -> fail_test "This should have failed make_directory ~parents:false"
    | Error (`system (`make_directory _, `already_exists)) -> return ()
    | Error e -> error e
    end
  end
  >>= fun () ->
  let path = Filename.concat tmp "some/very/long/path" in
  System.ensure_directory_path path
  >>= fun () ->
  ksprintf System.Shell.do_or_fail "find %s -type d" tmp
  >>= fun () ->
  say "test_make_directory: OK";
  return ()

let test_file_info () =
  let tmp = Filename.temp_dir "sys_test_file_info" "_dir" in
  ksprintf System.Shell.do_or_fail "cd %s && ln -s /tmp symlink_to_dir" tmp
  >>= fun () ->
  ksprintf System.Shell.do_or_fail "cd %s && ln -s /etc/passwd symlink_to_file" tmp
  >>= fun () ->
  let check ?follow_symlink matches path =
    System.file_info ?follow_symlink path
    >>= begin function
    | o when matches o -> return ()
    | e -> error (`wrong_file_info (path, e))
    end
  in
  check ((=) `directory) "/" >>= fun () ->
  check (function `regular_file _ -> true | _ -> false) "/etc/passwd" >>= fun () ->
  ksprintf (check ((=) (`symlink "/etc/passwd"))) "%s/symlink_to_file" tmp
  >>= fun () ->
  ksprintf (check ((=) (`symlink "/tmp"))) "%s/symlink_to_dir" tmp
  >>= fun () ->
  check ((=) `absent) "/sldkfjslakjfdlksj"
  >>= fun () ->

  ksprintf System.Shell.do_or_fail "ls -l %s " tmp
  >>= fun () ->
  say "test_file_info: OK";
  return ()

let is_present ?(and_matches=(fun _ -> true)) path =
  System.file_info path
  >>= begin function
  | `absent -> error (`wrong_file_info (path, `absent))
  | any when and_matches any -> return ()
  | any_other -> error (`wrong_file_info (path, any_other))
  end

let is_absent path =
  System.file_info path
  >>= begin function
  | `absent -> return ()
  | e -> error (`wrong_file_info (path, e))
  end

let random_tree path max_number_creations =
  let max_number_creations = ref max_number_creations in
  let rec random_tree_aux path =
    while_sequential (List.init (Random.int 20 + 20) ident) (fun id ->
    if !max_number_creations <= 0 then return ()
    else begin
      let path n =
        ksprintf (Filename.concat path) "random_%s_%d" n id in
      decr max_number_creations;
      begin match Random.int 3 with
      | 0 ->
        let p = path "dir" in
        System.ensure_directory_path p
        >>= fun () ->
        random_tree_aux p
      | 1 ->
        let size = Random.int 1_000_000 in
        let content = String.create size in
        let p = ksprintf path "file_of_size_%d" size in
        IO.write_file p ~content
      | _ ->
        let p = path "symlink" in
        System.make_symlink ~target:"/tmp/bouh" ~link_path:p
      end
    end)
    >>= fun _ ->
    return ()
  in
  random_tree_aux path

let test_remove style =
  let in_dir =
    match style with
    | `relative -> "test_flow_system_remove"
    | `absolute -> "/tmp/test_flow_system_remove" in
  cmdf "rm -fr %s" in_dir
  >>= fun () ->
  System.ensure_directory_path in_dir
  >>= fun () ->
  let test_regular = Filename.concat in_dir "reg_file" in
  IO.write_file test_regular ~content:"come content"
  >>= fun () ->
  System.file_info test_regular
  >>= begin function
  | `regular_file l when l > 2 -> return ()
  | e -> error (`wrong_file_info (test_regular, e))
  end
  >>= fun () ->
  System.remove test_regular
  >>= fun () ->
  say "Removed %s" test_regular;
  is_absent test_regular
  >>= fun () ->
  let test_empty_dir = Filename.concat in_dir "empty_dir" in
  System.ensure_directory_path test_empty_dir >>= fun () ->
  is_present test_empty_dir >>= fun () ->
  System.remove test_empty_dir >>= fun () ->
  is_absent test_empty_dir >>= fun () ->
  say "Removed %s" test_empty_dir;
  let test_non_empty_dir = Filename.concat in_dir "non_empty_dir" in
  System.ensure_directory_path test_non_empty_dir >>= fun () ->
  random_tree test_non_empty_dir 100
  >>= fun () ->
  cmdf "find %s | wc -l" test_non_empty_dir
  >>= fun () ->
  System.remove test_non_empty_dir >>= fun () ->
  is_absent test_non_empty_dir >>= fun () ->

  let test_symlink = Filename.concat in_dir "test_symlink" in
  System.make_symlink ~target:"/tmp/bouh" ~link_path:test_symlink
  >>= fun () ->
  is_present ~and_matches:((=) (`symlink "/tmp/bouh")) test_symlink >>= fun () ->
  System.remove test_symlink >>= fun () ->
  is_absent test_symlink >>= fun () ->
  say "Removed: %s" test_symlink;

  System.remove in_dir >>= fun () ->
  is_absent in_dir >>= fun () ->

  say "test_remove: OK";
  return ()


let check_error_file_exists name expected_path =
  begin function
  | Ok () -> fail_test "%s: default 'if_exists' should be `fail" name
  | Error (`system (_, (`file_exists p))) when p = expected_path -> return ()
  | Error e -> error e
  end

let test_copy style_in style_out =
  let out_dir =
    match style_out with
    | `relative -> "test_flow_system_copy_target"
    | `absolute -> "/tmp/test_flow_system_copy_target" in
  let in_dir =
    match style_in with
    | `relative -> "test_flow_system_copy"
    | `absolute -> "/tmp/test_flow_system_copy" in
  say "test_copy %s/… → %s/…" in_dir out_dir;
  cmdf "rm -fr %s" in_dir >>= fun () ->
  cmdf "rm -fr %s" out_dir >>= fun () ->
  System.ensure_directory_path in_dir >>= fun () ->
  System.ensure_directory_path out_dir >>= fun () ->

  (******************)
  (* Symbolic links *)
  (******************)

  let test_symlink = Filename.concat in_dir "test_symlink" in
  System.make_symlink ~target:"/tmp/bouh" ~link_path:test_symlink
  >>= fun () ->

  say "`redo symlinks %s into %s" test_symlink out_dir;
  System.copy ~symlinks:`redo ~src:test_symlink (`into out_dir)
  >>= fun () ->
  let expected_path = Filename.concat out_dir "test_symlink" in
  is_present ~and_matches:((=) (`symlink "/tmp/bouh")) expected_path
  >>= fun () ->

  System.copy ~symlinks:`redo ~src:test_symlink (`into out_dir)
  >>< check_error_file_exists "copy-redo-symlink" expected_path
  >>= fun () ->

  System.copy ~symlinks:`redo ~src:test_symlink ~if_exists:`overwrite (`into out_dir)
  >>= fun () ->

  say "redo symlink %s as %s" test_symlink out_dir;
  let dst = Filename.concat out_dir "test_symlink_new" in
  System.copy ~symlinks:`redo ~src:test_symlink (`onto dst)
  >>= fun () ->
  is_present ~and_matches:((=) (`symlink "/tmp/bouh")) dst
  >>= fun () ->
  System.remove dst (* we remove this one to be able to compare with
                       file_tree at the end *)
  >>= fun () ->

  (**************************************)
  (* Copying files (with various sizes) *)
  (**************************************)
  let test_copy_file size =
    let test_reg_file = Filename.concat in_dir (sprintf "reg_file_%d" size) in
    let content = String.make size 'B' in
    IO.write_file test_reg_file ~content
    >>= fun () ->
    System.copy ~src:test_reg_file (`into out_dir)
    >>= fun () ->
    let expected_path = Filename.(concat out_dir (basename test_reg_file)) in
    is_present ~and_matches:((=) (`regular_file size)) expected_path
    >>= fun () ->
    IO.read_file expected_path
    >>= fun content_got ->
    begin
      if content_got = content
      then return ()
      else
        fail_test "contents of %s and %s are different"
          test_reg_file expected_path
    end
    >>= fun () ->
    System.copy ~src:test_reg_file (`into out_dir)
    >>< check_error_file_exists "copy-file" expected_path
    >>= fun () ->
    (* This one should work: *)
    System.copy ~if_exists:`overwrite ~src:test_reg_file (`into out_dir)
  in
  test_copy_file 200 >>= fun () ->
  test_copy_file 200_000 >>= fun () ->

  (**************************************)
  (* Copying full trees of files        *)
  (**************************************)
  let subtree_path = Filename.concat in_dir "random_tree" in
  System.ensure_directory_path subtree_path
  >>= fun () ->
  random_tree subtree_path 20
  >>= fun () ->

  System.copy ~symlinks:`redo ~src:subtree_path (`into out_dir)
  >>= fun () ->

  System.copy ~src:subtree_path (`into out_dir)
  >>< check_error_file_exists "copy-whole-random-tree"
    Filename.(concat out_dir "random_tree")
  >>= fun () ->

  (* The file_tree comparison mostly shows that the copy worked and that
     the failing one did not add anything. *)
  let compare_file_trees in_dir out_dir fmt =
    System.file_tree ~follow_symlinks:false in_dir
    >>= fun in_tree ->
    System.file_tree ~follow_symlinks:false out_dir
    >>= fun out_tree ->
    begin match in_tree, out_tree with
    | `node (inname, lin), `node (outname, lout)
      when inname = Filename.basename in_dir
      && outname = Filename.basename out_dir
        && lin = lout ->
      return ()
    | _ ->
      say "in_tree: %s" (Sexp.to_string_hum (System.sexp_of_file_tree in_tree));
      say "out_tree: %s" (Sexp.to_string_hum (System.sexp_of_file_tree out_tree));
      ksprintf (fun s -> fail_test "in_tree <> out_tree %s" s) fmt
    end
  in
  compare_file_trees in_dir out_dir "after copy + copy-failure"
  >>= fun () ->

  (* `overwrite should succeed and leave exactly the same result. *)
  System.copy ~if_exists:`overwrite ~symlinks:`redo ~src:subtree_path (`into out_dir)
  >>= fun () ->
  compare_file_trees in_dir out_dir "after overwriting"
  >>= fun () ->

  (* `update should add/overwrite files but not remove *)
  System.file_tree ~follow_symlinks:false out_dir
  >>= fun init_dir ->
  System.remove subtree_path  >>= fun () ->
  is_absent subtree_path >>= fun () ->
  let another_directory = Filename.concat subtree_path "another_one" in
  System.ensure_directory_path another_directory  >>= fun () ->
  let one_file = Filename.concat another_directory "one_file" in
  IO.write_file one_file ~content:"AAAA"
  >>= fun () ->
  let another_file = Filename.concat subtree_path "file" in
  IO.write_file another_file ~content:"BBBB" >>= fun () ->
  (* We now have:
     subtree_path/another_one/one_file
     subtree_path/file *)
  System.file_tree ~follow_symlinks:false out_dir
  >>= fun out_tree_before ->
  System.copy ~if_exists:`update ~symlinks:`redo ~src:subtree_path (`into out_dir)
  >>= fun () ->
  System.file_tree ~follow_symlinks:false out_dir
  >>= fun out_tree ->
  let rec path_list path tree =
    match tree with
    | `leaf (f, _) -> [Filename.concat path f]
    | `node (n, l) ->
      let now = Filename.concat path n in
      now :: List.concat_map l ~f:(fun sub -> path_list now sub)
  in
  begin match out_tree_before, out_tree with
  | `node (dir, content_before), `node (same, content_after) when dir = same ->
    let paths_before = path_list "." out_tree_before in
    let paths_after = path_list "." out_tree in
    let check f =
      match List.find paths_after (fun p -> Filename.basename p = f) with
      | Some p -> return ()
      | None -> fail_test "paths_after does not have %s" f
    in
    check  "another_one" >>= fun () ->
    check  "one_file" >>= fun () ->
    check  "file" >>= fun () ->
    let included =
      List.for_all paths_before (fun p -> List.mem paths_after p) in
    if included then return ()
    else (
      say "old_paths:\n%s" (String.concat ~sep:"\n" paths_before);
      say "new_paths:\n%s" (String.concat ~sep:"\n" paths_after);
      fail_test "paths_before not-included-in paths_after"
    )
  | _ ->
    say "out_tree_before: %s" (Sexp.to_string_hum (System.sexp_of_file_tree out_tree_before));
    say "out_tree: %s" (Sexp.to_string_hum (System.sexp_of_file_tree out_tree));
    fail_test "out_tree_before <> out_tree (copy `update)"
  end
  >>= fun () ->

  System.remove in_dir >>= fun () ->
  System.remove out_dir >>= fun () ->
  say "test_copy: OK";
  return ()

let test_move style_in style_out =
  (*
    Note: to test the `move` function in the [`must_copy] case one has to
    launch this test from another partition than the one containing "/tmp".
  *)
  let out_dir =
    match style_out with
    | `relative -> "test_flow_system_move_target"
    | `absolute -> "/tmp/test_flow_system_move_target" in
  let in_dir =
    match style_in with
    | `relative -> "test_flow_system_move"
    | `absolute -> "/tmp/test_flow_system_move" in
  say "test_move %s/… → %s/…" in_dir out_dir;
  cmdf "rm -fr %s" in_dir >>= fun () ->
  cmdf "rm -fr %s" out_dir >>= fun () ->
  System.ensure_directory_path in_dir >>= fun () ->
  System.ensure_directory_path out_dir >>= fun () ->

  let test_symlink = Filename.concat in_dir "test_symlink" in
  System.make_symlink ~target:"/tmp/bouh" ~link_path:test_symlink
  >>= fun () ->
  System.move ~symlinks:`redo ~src:test_symlink (`into out_dir)
  >>= fun () ->
  let expected_path = Filename.concat out_dir "test_symlink" in
  is_present ~and_matches:((=) (`symlink "/tmp/bouh")) expected_path
  >>= fun () ->
  is_absent test_symlink >>= fun () ->

  let subtree_path = Filename.concat in_dir "random_tree" in
  System.ensure_directory_path subtree_path >>= fun () ->
  random_tree subtree_path 20 >>= fun () ->
  System.file_tree ~follow_symlinks:false subtree_path
  >>= fun src_tree ->

  let new_tree = Filename.concat out_dir "random_tree_moved" in
  System.move ~symlinks:`redo ~src:subtree_path (`onto new_tree)
  >>= fun () ->
  System.file_tree ~follow_symlinks:false new_tree
  >>= fun dst_tree ->
  is_absent subtree_path
  >>= fun () ->

  begin match src_tree, dst_tree with
  | `node (inname, lin), `node (outname, lout)
    when inname = "random_tree" && outname = "random_tree_moved"
                && lin = lout ->
    return ()
  | _ ->
    say "src_tree: %s" (Sexp.to_string_hum (System.sexp_of_file_tree src_tree));
    say "dst_tree: %s" (Sexp.to_string_hum (System.sexp_of_file_tree dst_tree));
    fail_test "src_tree <> dst_tree"
  end
  >>= fun () ->


  System.ensure_directory_path (Filename.concat in_dir "somedir")
  >>= fun () ->
  System.ensure_directory_path (Filename.concat out_dir "somedir")
  >>= fun () ->
  System.move (Filename.concat in_dir "somedir") (`into out_dir)
  >>< check_error_file_exists "move should fail if exists"
    (Filename.concat out_dir "somedir")
  >>= fun () ->
  System.move ~if_exists:`update ~src:(Filename.concat in_dir "somedir") (`into out_dir)
  >>= fun () ->

  System.remove in_dir >>= fun () ->
  System.remove out_dir >>= fun () ->
  say "test_move: OK";
  return ()

let test_shell () =
  let silent fmt =
    ksprintf (fun s ->
      ksprintf System.Shell.do_or_fail "( %s ) > /dev/null 2>&1" s) fmt in
  silent "ls /tmp"
  >>= fun () ->

  bind_on_error (silent "ls /some_big_path")
    begin function
    | `shell (_, `exited 2) -> return ()
    | e -> error e
    end
  >>= fun () ->

  bind_on_error (silent "kill $$")
    begin function
    | `shell (_, `signaled s) when s = Signal.term -> return ()
    | e -> error e
    end
  >>= fun () ->

  bind_on_error (silent "kill -9 $$")
    begin function
    | `shell (_, `signaled s) when s = Signal.kill -> return ()
    | e -> error e
    end
  >>= fun () ->


  let check_output ~ok fmt =
    ksprintf (fun s ->
      System.Shell.execute s
      >>= begin function
      | (sin, sout, ex) when ok sin sout ex -> return ()
      | (sin, sout, ex) ->
        fail_test "output of '%s':\n%S\n%S\n%S" s sin sout
          (<:sexp_of< [ `exited of int | `signaled of Signal.t | `stopped of int ] >>
              ex |! Sexp.to_string_hum)
      end
    ) fmt  in

  check_output "ls /"
    ~ok:(fun sin sout ex ->
      ex = `exited 0 && sout = "" && String.length sin > 10)
  >>= fun () ->

  check_output "ls /some_big_path"
    ~ok:(fun sin sout ex -> ex = `exited 2)
  >>= fun () ->

  check_output "kill -9 $$" ~ok:(fun _ _ ex -> ex = `signaled Signal.kill)
  >>= fun () ->

  check_output "echo 'bouh'; exit 2"
    ~ok:(fun sin sout ex ->
      ex = `exited 2
      && sin = "bouh\n")
  >>= fun () ->

  let buffer_size = Lwt_io.default_buffer_size () in
  check_output "for i in `seq 1 %d`; do echo '===============' ; done"
    buffer_size
    ~ok:(fun sin sout ex ->
      String.length sin > buffer_size * 10
      && ex = `exited 0)
  >>= fun () ->

  say "test_shell: OK";
  return ()

let main () =
  say "sys_test: GO!";
  let (>>=) m f =
    m >>= (fun o ->
      say "=======================================";
      f o) in
  test_make_directory ()
  >>= fun () ->
  test_file_info ()
  >>= fun () ->
  test_remove `relative >>= fun () ->
  test_remove `absolute >>= fun () ->
  test_copy `relative `relative  >>= fun () ->
  test_copy `absolute `absolute  >>= fun () ->
  test_copy `absolute `relative  >>= fun () ->
  test_copy `relative `absolute  >>= fun () ->
  test_move `relative `relative  >>= fun () ->
  test_move `absolute `absolute  >>= fun () ->
  test_move `absolute `relative  >>= fun () ->
  test_move `relative `absolute  >>= fun () ->
  test_shell () >>= fun () ->
  say "sys_test: Successful End";
  return ()

let () =
  match Lwt_main.run (main ()) with
  | Ok () -> ()
  | Error e ->
    eprintf "End with Error:\n%s\n%!"
      (<:sexp_of<
          [ `system of
              [`file_info of string | `make_directory of string
              | `file_tree of string
              | `move of string
              | `copy of string
              | `make_symlink of string * string
              | `remove of string | `list_directory of string ] *
                [ `already_exists
                | `file_not_found of string
                | `not_a_directory of string
                | `file_exists of string
                | `wrong_path of string
                | `wrong_file_kind of string *
                    [ `block_device | `character_device | `fifo | `socket | `symlink of string ]
                | `exn of exn | `wrong_access_rights of int ]
          | `failed_test of string
          | `write_file_error of string * exn
          | `read_file_error of string * exn
          | `file_exists of string
          | `wrong_path of string
          | `wrong_file_info of string *
              [ `absent
              | `block_device
              | `character_device
              | `directory
              | `fifo
              | `regular_file of int
              | `socket
              | `symlink of string ]
          | `shell of
              string *
                [ `exited of int
                | `exn of exn
                | `signaled of Signal.t
                | `stopped of int ]
          ]  >> e
       |! Sexp.to_string_hum)
