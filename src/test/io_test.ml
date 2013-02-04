open Core.Std
open Flow

let say fmt =
  ksprintf (fun s -> eprintf "%s\n%!" s) fmt

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
  let s2 = IO.Transform.transform_stream rot13 s1 in
  wrap_io Lwt_stream.to_list s2
  >>= fun l2 ->
  while_sequential l2 (fun s -> say "rot13ed: %s" s; return ())
  >>= fun _ ->
  return ()
      

let main () =
  basic_file_to_file ()
  >>= fun () ->
  basic_stream ()
    
let () =
  let module E = struct
    type t = [
    | `io_exn of exn
    | `read_file_error of string * exn
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
              
