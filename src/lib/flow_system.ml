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
open Flow_list

module IO = Flow_io

let wrap_deferred_system f =
  wrap_deferred f ~on_exn:(fun e -> `system_exn e)

module Shell = struct

  let discriminate_process_status s ret =
    begin match ret with
    | Lwt_unix.WEXITED 0 -> return ()
    | Lwt_unix.WEXITED n -> error (`shell (s, `exited n))
    | Lwt_unix.WSIGNALED n ->
      error (`shell (s, `signaled (Signal.of_caml_int n)))
    | Lwt_unix.WSTOPPED n -> error (`shell (s, `stopped n))
    end

  let do_or_fail s =
    bind_on_error ~f:(fun e -> error (`shell (s, `exn e)))
      (catch_deferred Lwt_io.(fun () -> Lwt_unix.system s))
    >>= fun ret ->
    discriminate_process_status s ret


  let execute s =
    bind_on_error
      ~f:(fun e -> error (`shell (s, `exn e)))
      (catch_deferred
         Lwt.(fun () ->
           let inprocess = Lwt_process.(open_process_full (shell s)) in
           Lwt_list.map_p Lwt_io.read
             [inprocess#stdout; inprocess#stderr; ]
           >>= fun output ->
           inprocess#status >>= fun status ->
           return (status, output)))
    >>= fun (ret, output) ->
    let code =
      match ret with
      | Lwt_unix.WEXITED n ->   (`exited n)
      | Lwt_unix.WSIGNALED n -> (`signaled (Signal.of_caml_int n))
      | Lwt_unix.WSTOPPED n ->  (`stopped n)
    in
    begin match output with
    | [out; err] -> return (out, err, code)
    | _ -> assert false
    end

end
let sleep f =
  wrap_deferred_system (fun () -> Lwt_unix.sleep f)

let with_timeout time ~f =
  Lwt.catch
    begin fun () ->
      Lwt_unix.with_timeout time f
    end
    begin function
    | Lwt_unix.Timeout -> error (`timeout time)
    | e -> error (`system_exn e)
    end


let mkdir_or_fail ?(perm=0o700) dirname =
  Lwt.catch
    Lwt.(fun () -> Lwt_unix.mkdir dirname perm >>= fun () -> return (Ok ()))
    begin function
    | Unix.Unix_error (Unix.EACCES, cmd, arg)  ->
      error (`system (`make_directory dirname, `wrong_access_rights perm))
    | Unix.Unix_error (Unix.EEXIST, cmd, arg)  ->
      error (`system (`make_directory dirname, `already_exists))
    | Unix.Unix_error (Unix.EISDIR, cmd, arg)  ->
      (* Bypass MacOSX bug https://github.com/janestreet/core/issues/7 *)
      error (`system (`make_directory dirname, `already_exists))
    | e ->
      error (`system (`make_directory dirname, `exn e))
    end

let mkdir_even_if_exists ?(perm=0o700) dirname =
  Lwt.catch
    Lwt.(fun () -> Lwt_unix.mkdir dirname perm >>= fun () -> return (Ok ()))
    begin function
    | Unix.Unix_error (Unix.EACCES, cmd, arg)  ->
      error (`system (`make_directory dirname, `wrong_access_rights perm))
    | Unix.Unix_error (Unix.EISDIR, cmd, arg)  ->
      (* Bypass MacOSX bug https://github.com/janestreet/core/issues/7 *)
      return ()
    | Unix.Unix_error (Unix.EEXIST, cmd, arg)  -> return ()
    | e -> error (`system (`make_directory dirname, `exn e))
    end

let make_new_directory ?perm dirname =
  mkdir_or_fail ?perm dirname

let ensure_directory_path ?perm dirname =
  (* Code inspired by Core.Std.Unix *)
  let init, dirs =
    match Filename.parts dirname with
    | [] -> failwithf "Sys.mkdir_p: BUG! Filename.parts %s -> []" dirname ()
    | init :: dirs -> (init, dirs)
  in
  mkdir_even_if_exists ?perm init
  >>= fun () ->
  List.fold dirs ~init:(return init) ~f:(fun m part ->
    m >>= fun previous ->
    let dir = Filename.concat previous part in
    mkdir_even_if_exists ?perm dir
    >>= fun () ->
      return dir)
  >>= fun _ ->
  return ()

type file_info =
[ `absent
| `regular_file of int
| `symlink of string
| `block_device
| `character_device
| `directory
| `fifo
| `socket]
with sexp

(*
  WARNING: this is a work-around for issue [329] with Lwt_unix.readlink.
  When it is fixed, we should go back to Lwt_unix.

  [329]: http://ocsigen.org/trac/ticket/329
*)
let lwt_unix_readlink l =
  let open Lwt in
  Lwt_preemptive.detach Unix.readlink l

let file_info ?(follow_symlink=false) path =
  let stat_fun =
    if follow_symlink then Lwt_unix.stat else Lwt_unix.lstat in
  (* eprintf "(l)stat %s? \n%!" path; *)
  Lwt.catch
    Lwt.(fun () -> stat_fun path >>= fun s -> return (Ok (`unix_stats s)))
    begin function
    | Unix.Unix_error (Unix.ENOENT, cmd, arg)  -> return `absent
    | e -> error (`system (`file_info path, `exn e))
    end
  >>= fun m ->
  let open Lwt_unix in
  begin match m with
  | `absent -> return `absent
  | `unix_stats stats ->
    begin match stats.st_kind with
    | S_DIR -> return (`directory)
    | S_REG -> return (`regular_file (stats.st_size))
    | S_LNK ->
      (* eprintf "readlink %s? \n%!" path; *)
      begin
        Flow_base.catch_deferred (fun () -> lwt_unix_readlink path)
        >>< begin function
        | Ok s -> return s
        | Error e -> error (`system (`file_info path, `exn e))
        end
      end
      >>= fun destination ->
      (* eprintf "readlink %s worked \n%!" path; *)
      return (`symlink destination)
    | S_CHR -> return (`character_device)
    | S_BLK -> return (`block_device)
    | S_FIFO -> return (`fifo)
    | S_SOCK -> return (`socket)
    end
  end

let list_directory path =
  let f_stream = Lwt_unix.files_of_directory path in
  let next s =
    catch_deferred Lwt.(fun () ->
      catch (fun () -> Lwt_stream.next s >>= fun n -> return (Some n))
        (function Lwt_stream.Empty -> return None
        | e -> fail e)
    ) in
  `stream (fun () ->
    bind_on_error (next f_stream)
      ~f:(fun e -> error (`system (`list_directory path, `exn e))))

let remove path =
  let rec remove_aux path =
    file_info path
    >>= begin function
    | `absent -> return ()
    | `block_device
    | `character_device
    | `symlink _
    | `fifo
    | `socket
    | `regular_file _-> wrap_deferred_system (fun () -> Lwt_unix.unlink path)
    | `directory ->
      let `stream next_dir = list_directory path in
      let rec loop () =
        next_dir ()
        >>= begin function
        | Some ".."
        | Some "." -> loop ()
        | Some name ->
          remove_aux (Filename.concat path name)
          >>= fun () ->
          loop ()
        | None -> return ()
        end
      in
      loop ()
      >>= fun () ->
      wrap_deferred_system (fun () -> Lwt_unix.rmdir path)
    end
  in
  remove_aux path
  >>< begin function
  | Ok () -> return ()
  | Error (`system_exn e) -> error (`system (`remove path, `exn e))
  | Error (`system e) -> error (`system e)
  end

let make_symlink ~target ~link_path =
  bind_on_error
    (wrap_deferred_system (fun () -> Lwt_unix.symlink target link_path))
    begin function
    | `system_exn e ->
      begin match e with
      | Unix.Unix_error (Unix.EEXIST, cmd, arg)  ->
        error (`system (`make_symlink (target, link_path), `file_exists link_path))
      | e -> error (`system (`make_symlink (target, link_path), `exn e))
      end
    end

type file_destination = [
| `into of string
| `onto of string
]
let path_of_destination ~src ~dst =
  match dst with
  | `into p -> Filename.(concat p (basename src))
  | `onto p -> p

let copy
    ?(ignore_strange=false) ?(symlinks=`fail) ?(buffer_size=64_000)
    ?(if_exists=`fail)
    ~src dst =
  let rec copy_aux ~src ~dst =
    file_info src
    >>= begin function
    | `absent -> error (`file_not_found src)
    | `block_device
    | `character_device
    | `fifo
    | `socket as k ->
      if ignore_strange then return () else error (`wrong_file_kind (src, k))
    | `symlink content ->
      begin match symlinks with
      | `fail -> error (`wrong_file_kind (src, `symlink content))
      | `follow -> copy_aux ~src:content ~dst
      | `redo ->
        let link_path = path_of_destination ~src ~dst in
        begin match if_exists with
        | `fail -> (* make_symlink already fails on existing files *)
          return ()
        | `overwrite
        | `update -> remove link_path (* remove does not fail on missing files *)
        end
        >>= fun () ->
        make_symlink ~target:content ~link_path
      end
    | `regular_file _->
      let output_path = path_of_destination ~src ~dst in
      let open_spec =
        match if_exists with
        | `fail -> `create_file output_path
        | `overwrite | `update -> `overwrite_file output_path
      in
      IO.with_out_channel ~buffer_size open_spec ~f:(fun outchan ->
        IO.with_in_channel ~buffer_size (`file src) ~f:(fun inchan ->
          let rec loop () =
            IO.read ~count:buffer_size inchan
            >>= begin function
            | "" -> return ()
            | buf ->
              IO.write outchan buf >>= fun () ->
              loop ()
            end
          in
          loop ()))
    | `directory ->
      let new_dir = path_of_destination ~src ~dst in
      file_info new_dir
      >>= begin function
      | `absent ->
        make_new_directory new_dir
      | smth_else ->
        begin match if_exists with
        | `fail -> error (`file_exists new_dir)
        | `overwrite ->
          remove new_dir
          >>= fun () ->
          make_new_directory new_dir
        | `update ->
          if smth_else = `directory
          then return ()
          else error (`not_a_directory new_dir)
        end
      end
      >>= fun () ->
      let `stream next_dir = list_directory src in
      let rec loop () =
        next_dir ()
        >>= begin function
        | Some ".."
        | Some "." -> loop ()
        | Some name ->
          copy_aux
            ~src:(Filename.concat src name)
            ~dst:(`into new_dir)
          >>= fun () ->
          loop ()
        | None -> return ()
        end
      in
      loop ()
    end
  in
  bind_on_error (copy_aux ~src ~dst)
    begin function
    | `io_exn e -> error (`system (`copy src, `exn e))
    | `file_exists _
    | `wrong_path _
    | `file_not_found _
    | `not_a_directory _
    | `wrong_file_kind _ as e -> error (`system (`copy src, e))
    | `system e -> error (`system e)
    end

let move_in_same_device ?(if_exists=`fail) ~src dst =
  let real_dest = path_of_destination ~src ~dst in
  begin match if_exists with
  | `fail ->
    file_info real_dest
    >>= begin function
    | `absent -> return ()
    | _ -> error (`system (`move src, `file_exists real_dest))
    end
  | _ -> (* Unix.rename does overwriting *) return ()
  end
  >>= fun () ->
  Lwt.catch
    Lwt.(fun () -> Lwt_unix.rename src real_dest >>= fun () -> return (Ok `moved))
    begin function
    | Unix.Unix_error (Unix.EXDEV, cmd, arg)  -> return `must_copy
    | Unix.Unix_error (Unix.ENOTEMPTY, cmd, arg)  -> return `must_copy
    | e -> error (`system (`move src, `exn e))
    end

let move ?ignore_strange ?symlinks ?buffer_size ?if_exists ~src dst =
  move_in_same_device ?if_exists ~src dst
  >>= begin function
  | `moved -> return ()
  | `must_copy ->
    copy ~src ?buffer_size ?ignore_strange ?symlinks ?if_exists dst
    >>= fun () ->
    remove src
  end


type file_tree = [
| `node of string * file_tree list
| `leaf of string * file_info
] with sexp

let file_tree ?(follow_symlinks=false) path =
  let directory p l = return (`node (p, l)) in
  let file p l = return (`leaf (p, l)) in
  let rec find_aux path =
    file_info path
    >>= begin function
    | `absent -> error (`file_not_found path)
    | `block_device
    | `character_device
    | `fifo
    | `regular_file _
    | `socket as k -> file (Filename.basename path) k
    | `symlink content as k ->
      begin match follow_symlinks with
      | true ->
        let continue =
          if Filename.is_relative content
          then (Filename.concat path content)
          else content in
        find_aux continue
      | false -> file (Filename.basename path) k
      end
    | `directory ->
      let `stream next_dir = list_directory path in
      let rec loop acc =
        next_dir ()
        >>= begin function
        | Some ".."
        | Some "." -> loop acc
        | Some name -> loop (name :: acc)
        | None -> return acc
        end
      in
      loop []
      >>= fun sub_list ->
      while_sequential (List.sort ~cmp:String.compare sub_list) (fun dir ->
        find_aux (Filename.concat path dir))
      >>= fun sub_tree ->
      directory (Filename.basename path) sub_tree
    end
  in
  bind_on_error (find_aux path)
    begin function
    | `io_exn e -> error (`system (`file_tree path, `exn e))
    | `file_not_found _ as e -> error (`system (`file_tree path, e))
    | `system e -> error (`system e)
    end
