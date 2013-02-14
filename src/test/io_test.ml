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
open Flow

let say fmt =
  ksprintf (fun s -> eprintf "%s\n%!" s) fmt

let wrap_deferred_io f =
  wrap_deferred (fun () -> f ()) ~on_exn:(fun e -> `io_test_exn e)

let basic_file_to_file () =
  let t1 = object
    val mutable q = (Fqueue.empty: string Fqueue.t)
    val mutable stopped = false
    method next =
      match Fqueue.dequeue q with
      | Some (o, qq) -> q <- qq; `output (Ok o)
      | None -> if stopped then `end_of_stream else `not_ready
    method feed s =
      List.iter (String.split ~on:'a' s) (fun portion ->
        q <- Fqueue.enqueue q (sprintf "portion: %s\n" portion))
    method stop = stopped <- true
  end in
  IO.Transform.file_to_file t1
    ~input_buffer_size:24 "/etc/passwd"
    ~output_buffer_size:23 "/tmp/portions"
  >>= fun () ->
  IO.read_file "/tmp/portions"
  >>= fun content ->
  say " THE FILE:\n%s\n" content;
  return ()

let basic_stream () =
  let rot13 = object
    val mutable q = (Fqueue.empty: string Fqueue.t)
    val mutable stopped = false
    method next =
      match Fqueue.dequeue q with
      | Some (o, qq) -> q <- qq; `output o
      | None -> if stopped then `end_of_stream else `not_ready
    method feed s =
      let rot13ed =
        let ch base c =
          let zero = Char.to_int base in
          Char.of_int_exn (zero + (((Char.to_int c mod zero) + 13) mod 26))
        in
        String.map s (function
        | 'a' .. 'z' as c -> ch 'a' c
        | 'A' .. 'Z' as c -> ch 'A' c
        | c -> c) in
      q <- Fqueue.enqueue q rot13ed
    method stop = stopped <- true
  end in
  let s1 = Lwt_stream.of_list ["hello"; "uryyb"] in
  let s2 = IO.Transform.to_stream_fun rot13 s1 in
  wrap_deferred_io (fun () -> Lwt_stream.to_list s2)
  >>= fun l2 ->
  while_sequential l2 (fun s -> say "rot13ed: %s" s; return ())
  >>= fun _ ->
  return ()

let copy () =
  let tmp = Filename.temp_file "io_test_copy" ".bin" in
  IO.write_file tmp ~content:"foo!"
  >>= fun () ->
  IO.with_in_channel (`file tmp) ~buffer_size:42 ~f:(fun i ->
    IO.read i
    >>= fun content ->
    IO.with_out_channel (`stdout) ~f:(fun o ->
      ksprintf (IO.write o) "Content of %s is %S\n" tmp content))


let fail_test fmt =
  ksprintf (fun s -> error (`test_failed s)) fmt

let fail_test_if cond fmt =
  ksprintf (fun s ->
    if cond then error (`test_failed s) else return ()) fmt

let test_with_out_channel () =
  let tmp = Filename.temp_file "io_test_with_out_channel" ".bin" in
  IO.write_file tmp ~content:"A"
  >>= fun () ->
  IO.with_out_channel (`append_to_file tmp) ~f:(fun o ->
    IO.write o "B")
  >>= fun () ->
  IO.read_file tmp
  >>= fun content ->
  fail_test_if (content <> "AB") "append_to_file"
  >>= fun () ->
  begin
    IO.with_out_channel (`create_file tmp) ~f:(fun o ->
      IO.write o "B")
    >>< begin function
    | Ok () -> fail_test "test_with_out_channel.create: could write in %s" tmp
    | Error (`file_exists p) -> return ()
    | Error (`io_exn e) ->
      eprintf "io_exn: %s\n%!" Exn.(to_string e);
      error (`io_exn e)
    | Error e ->
      error e
    end
  end
  >>= fun () ->
  System.remove tmp >>= fun () ->

  IO.with_out_channel (`create_file tmp) ~f:(fun o ->
    IO.write o "AB")
  >>= fun () ->

  IO.read_file tmp >>= fun content ->
  fail_test_if (content <> "AB") "effectively create"
  >>= fun () ->

  IO.with_out_channel (`overwrite_file tmp) ~f:(fun o ->
    IO.write o "CD")
  >>= fun () ->

  IO.read_file tmp >>= fun content ->
  fail_test_if (content <> "CD") "overwrite_file"
  >>= fun () ->

  say "test_with_out_channel: OK";
  return ()


let main () =
  basic_file_to_file ()
  >>= fun () ->
  basic_stream ()
  >>= fun () ->
  copy ()
  >>= fun () ->
  test_with_out_channel ()

let () =
  let module E = struct
    type t = [
    | `io_exn of exn
    | `io_test_exn of exn
    | `test_failed of string
    | `read_file_error of string * exn
    | `write_file_error of string * exn
    | `file_exists of string
    | `wrong_path of string
    | `system of
        [ `file_info of string
        | `list_directory of string
        | `remove of string ] *
          [ `exn of exn ]
    | `transform of
        [ `io_exn of exn
        | `stopped_before_end_of_stream
        | `transform_error of unit ]
    ] with sexp_of
  end in
  match Lwt_main.run (main ()) with
  | Ok () -> ()
  | Error e ->
    eprintf "End with Error:\n%s\n%!" (E.sexp_of_t e |! Sexp.to_string_hum)

