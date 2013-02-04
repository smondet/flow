open Core.Std
open Flow_base

(******************************************************************************)
(* Binary Messages *)

let max_message_length = 10_000_000
  
let bin_send oc msg =
  if String.length msg > max_message_length then
    error (`bin_send (`message_too_long msg))
  else
    catch_io msg
      ~f:Lwt.(fun s ->
        Lwt_io.BE.write_int oc (String.length s) >>= fun () ->
        Lwt_io.write oc s >>= fun () ->
        Lwt_io.flush oc)
    |! bind_on_error ~f:(fun e -> error (`bin_send (`exn e)))

let bin_recv ic =
  let io =
    catch_io () ~f:Lwt.(fun () ->
      Lwt_io.BE.read_int ic >>= fun c ->
      begin if max_message_length >= c && c > 0 then (
        let s = String.make c 'B' in
        Lwt_io.read_into_exactly ic s 0 c >>= fun () ->
        return s
      ) else
          return ""
      end
      >>= fun s ->
      return (c,s)) in
  bind_on_error io (fun e -> error (`bin_recv (`exn e)))
  >>= fun (c, s) ->
  if String.length s <> c then
    error (`bin_recv (`wrong_length (c, s)))
  else
    return s

(******************************************************************************)      
(* Channels *)

let with_out_channel out ?buffer_size ~f =
  begin match out with
  | `stdout -> return Lwt_io.stdout
  | `strerr -> return Lwt_io.stderr
  | `channel c -> return c
  | `file file ->
    wrap_io (Lwt_io.open_file ~mode:Lwt_io.output ?buffer_size) file
  end
  >>= fun outchan ->
  begin
    f outchan
    >>< begin function
    | Ok o ->
      wrap_io Lwt_io.close outchan
      >>= fun () ->
      return o
    | Error e ->
      begin match out with
      | `file _ ->
        wrap_io Lwt_io.close outchan
        >>= fun _ ->
        error e
      | _ -> error e
      end
    end
  end

let output out s =
  wrap_io (Lwt_io.fprint out) s
    
let flush out = wrap_io Lwt_io.flush out


let with_in_channel inspec ?buffer_size ~f =
  begin match inspec with
  | `stdin -> return Lwt_io.stdin
  | `channel c -> return c
  | `file file ->
    wrap_io (Lwt_io.open_file ~mode:Lwt_io.input ?buffer_size) file
  end
  >>= fun inchan ->
  begin
    f inchan
    >>< begin function
    | Ok o ->
      wrap_io Lwt_io.close inchan
      >>= fun () ->
      return o
    | Error e ->
      begin match inspec with
      | `file _ ->
        wrap_io Lwt_io.close inchan
        >>= fun _ ->
        error e
      | _ -> error e
      end
    end
  end
  
    
(******************************************************************************)
(* Whole Files *)

let  write_file file ~content =
  catch_io () ~f:Lwt_io.(fun () ->
    with_file ~mode:output file (fun i -> write i content))
  |! bind_on_error ~f:(fun e -> error (`write_file_error (file, e)))

let read_file file =
  catch_io () ~f:Lwt_io.(fun () ->
    with_file ~mode:input file (fun i -> read i))
  |! bind_on_error ~f:(fun e -> error (`read_file_error (file, e)))

(******************************************************************************)
(* Biocaml/Crytokit-style transforms *)

module Transform = struct

  class type ['input, 'output] t = object
    method next: [ `output of 'output | `end_of_stream | `not_ready ]
    method feed:  'input -> unit
    method stop: unit
  end

  let augment_error = function
    | Ok o -> return o
    | Error e -> error (`transform e)
        
  let transform_stream tr stream =
    let open Lwt in
    let rec loop_until_ready tr stream =
      match tr#next with
      | `output o -> return (Some o)
      | `end_of_stream -> return None
      | `not_ready ->
        Lwt_stream.get stream
        >>= begin function
        | None -> tr#stop; loop_until_ready tr stream
        | Some s -> tr#feed s; loop_until_ready tr stream
        end
    in
    Lwt_stream.from (fun () -> loop_until_ready tr stream)

  let file_to_file
      (type error) (transfo: (string, (string, error) Result.t) t)
      ?(input_buffer_size=42_000) input_file
      ?(output_buffer_size=42_000) output_file =
    let module With_exceptions = struct
      exception Transform of error
      exception Premature_termination
      open Lwt
      open Lwt_io
      let go () =
        with_file ~mode:input ~buffer_size:input_buffer_size input_file (fun i ->
          with_file ~mode:output ~buffer_size:output_buffer_size output_file (fun o ->
            let rec print_all stopped =
              match transfo#next with
              | `output (Ok s) ->
                write o s
                >>= fun () ->
                print_all stopped
              | `end_of_stream ->
                if stopped then
                (* Lwt_io.eprintf "=====  WELL TERMINATED \n%!" *)
                  return ()
                else begin
                (* Lwt_io.eprintf "=====  PREMATURE TERMINATION \n%!" >>= fun () -> *)
                  fail (Premature_termination)
                end
            | `not_ready ->
              (* dbg "NOT READY" >>= fun () -> *)
              if stopped then print_all stopped else return ()
            | `output (Error (e)) ->
              (* Lwt_io.eprintf "=====  ERROR: %s\n%!" s *)
              fail (Transform e)
            in
            let rec loop () =
              read ~count:input_buffer_size i
              >>= fun read_string ->
            (* dbg verbose "read_string: %d" (String.length read_string) *)
            (* >>= fun () -> *)
              if read_string = "" then (
                transfo#stop;
                print_all true
              ) else (
                transfo#feed read_string;
                print_all false
                >>= fun () ->
                loop ()
              )
            in
            loop ()
          >>= fun () ->
            return (Ok ())
          ))
      let go_safe () =
        Lwt.catch go
          begin function
          | Premature_termination -> Flow_base.error `stopped_before_end_of_stream
          | Transform e -> Flow_base.error (`transform_error e)
          | e -> Flow_base.error (`io_exn e)
          end
    end in
    With_exceptions.go_safe ()
    >>< augment_error

end
