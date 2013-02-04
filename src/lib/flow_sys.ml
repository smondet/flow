open Core.Std
open Flow_base
  

let discriminate_process_status s ret =
  begin match ret with
  | Lwt_unix.WEXITED 0 -> return ()
  | Lwt_unix.WEXITED n -> error (`system_command_error (s, `exited n))
  | Lwt_unix.WSIGNALED n -> error (`system_command_error (s, `signaled n))
  | Lwt_unix.WSTOPPED n -> error (`system_command_error (s, `stopped n))
  end
      
let system_command s =
  bind_on_error ~f:(fun e -> error (`system_command_error (s, `exn e)))
    (catch_io () ~f:Lwt_io.(fun () -> Lwt_unix.system s))
  >>= fun ret ->
  discriminate_process_status s ret

let sleep f =
  wrap_io Lwt_unix.sleep f

    
let get_system_command_output s =
  bind_on_error ~f:(fun e -> error (`system_command_error (s, `exn e)))
    (catch_io
       Lwt.(fun () ->
         let inprocess = Lwt_process.(open_process_full (shell s)) in
         Lwt_list.map_p Lwt_io.read
           [inprocess#stdout; inprocess#stderr; ]
         >>= fun output ->
         inprocess#status >>= fun status ->
         return (status, output))
       ())
  >>= fun (ret, output) ->
  discriminate_process_status s ret
  >>= fun () ->
  begin match output with
  | [out; err] -> return (out, err)
  | _ -> assert false
  end

let with_timeout time ~f =
  Lwt.catch
    begin fun () ->
      Lwt_unix.with_timeout time f
    end
    begin function
    | Lwt_unix.Timeout -> error (`timeout time)
    | e -> error (`io_exn e)
    end
