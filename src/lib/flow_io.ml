open Core.Std
open Flow_base

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
      
