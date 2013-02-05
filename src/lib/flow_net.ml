open Core.Std
open Flow_base
open Flow_list
open Flow_system

let dbg fmt =
  ksprintf (fun s ->
    let indented =
      s |! String.split ~on:'\n' |! String.concat ~sep:"\n          " in
    wrap_io (Lwt_io.eprintf "DEBUG:   %s\n%!") indented) fmt

module Tls = struct

  let accept socket context =
    bind_on_error
      (catch_io (Lwt_ssl.ssl_accept socket) context)
      (fun e -> error (`tls_accept_error e))

  let server_socket ~port =
    let open Lwt_unix in
    try
    let fd = socket PF_INET SOCK_STREAM 6 in
    bind fd (ADDR_INET (Unix.Inet_addr.bind_any, port));
    listen fd port;
    return fd
    with
    | e -> error (`socket_creation_exn e)

  let tls_connect socket ssl_context =
    wrap_io (Lwt_ssl.ssl_connect socket) ssl_context

  let tls_shutdown socket =
    wrap_io Lwt_ssl.ssl_shutdown socket >>= fun () ->
    wrap_io (fun  () ->
      Lwt_ssl.shutdown socket Lwt_unix.SHUTDOWN_ALL;
      Lwt.return ()) () >>= fun () ->
    wrap_io Lwt_ssl.close socket

  module M_ugly_ssl_get_certificate = struct
    type ttt = Plain | SSL of Ssl.socket
    type lwt_ssl_socket = Lwt_unix.file_descr * ttt

    let get_certificate sslsock =
      begin match snd (Obj.magic sslsock : lwt_ssl_socket) with
      | Plain -> error (`not_an_ssl_socket)
      | SSL s ->
        Lwt.(
          catch
            (fun () ->
              Lwt_preemptive.detach Ssl.get_certificate s >>= fun cert ->
              return (Ok cert))
            (function
            | Ssl.Certificate_error ->
              return (Error `ssl_certificate_error)
            | e ->
              return (Error (`io_exn e))))
      end
  end
  let tls_get_certificate = M_ugly_ssl_get_certificate.get_certificate


  let server_context ?ca_certificate (cert_file, key_file) =
    let open Ssl in
    try
      let c = create_context TLSv1 Server_context in
      use_certificate c cert_file key_file;
      set_cipher_list c "TLSv1";
      Option.iter ca_certificate (fun ca_cert ->
        set_verify c [ Verify_peer; ] None;
        set_verify_depth c 99;
        load_verify_locations c ca_cert "";
      );
      return c
    with e -> error (`tls_context_exn e)


  let accept_loop
      ?(on_error: 'a -> (unit, [> ]) Flow_base.t =fun e -> return ())
      ?check_client_certificate ?tls_context ~port f =
    server_socket ~port >>= fun socket ->
    let handle_one accepted =
      begin match tls_context with
      | Some ssl_context ->
        accept (fst accepted) ssl_context
      | None -> return (Lwt_ssl.plain (fst accepted))
      end
      >>= fun ssl_accepted ->
      begin match check_client_certificate with
      | Some ccc ->
        double_bind (tls_get_certificate ssl_accepted)
          ~error:(function
          | `ssl_certificate_error ->
            return (`invalid_client `wrong_certificate)
          | `not_an_ssl_socket | `io_exn _ as e -> error e)
          ~ok:(fun cert ->
            ccc cert >>= function
            | `valid name -> return (`valid_client (name: string))
            | `expired (name, time) ->
              return (`invalid_client (`expired ((name: string), (time: Time.t))))
            | `revoked (name, time) ->
              return (`invalid_client (`revoked ((name: string), (time: Time.t))))
            | `not_found name ->
              return (`invalid_client (`not_found (name: string))))
      | None -> return `anonymous_client
      end
      >>= fun client ->
      f ssl_accepted client
    in
    let rec accept_loop c =
      wrap_io (Lwt_unix.accept_n socket) 10
      >>= fun (accepted_list, potential_exn) ->
      map_option potential_exn (fun exn -> on_error (`accept_exn exn))
      >>= fun (_ : unit option) ->
      (* dbg "Accepted %d connections (unix)%s" (List.length accepted_list) *)
      (*   (Option.value_map ~default:"" potential_exn *)
      (*      ~f:(fun e -> sprintf ", Exn: %s" (Exn.to_string e))) *)
      (* >>= fun () -> *)
      accept_loop (c + 1) |! Lwt.ignore_result;
      Lwt.(
        Lwt_list.map_p handle_one accepted_list
        >>= fun res_l ->
        Lwt_list.map_p (function
        | Ok () -> return (Ok ())
        | Error e -> on_error e) res_l
        >>= fun _ ->
        return (Ok ()))
    in
    accept_loop 0 |! Lwt.ignore_result;
    return ()

  let client_context ?verification_policy kind =
    let open Ssl in
    begin
      try
        let c = create_context TLSv1 Client_context in
        begin match kind with
        | `anonymous -> ()
        | `with_certificate (cert, key) ->
          use_certificate c cert key
        end;
        set_cipher_list c "TLSv1";
        Option.iter verification_policy (function
        | `verify_server ->
          Ssl.set_verify_depth c 99;
          set_verify c [Verify_peer] (Some client_verify_callback);
        | `allow_self_signed -> ()
        );
        return c
      with e -> error (`tls_context_exn e)
    end


end

let init_tls = Ssl.init ~thread_safe:true

type connection = {
  inchan: Lwt_io.input_channel;
  outchan: Lwt_io.output_channel;
  tls_socket: Lwt_ssl.socket;
}
let connection inchan outchan tls_socket =
  {inchan; outchan; tls_socket}

let in_channel t = t.inchan
let out_channel t = t.outchan
let shutdown {tls_socket; inchan; outchan} =
  Tls.tls_shutdown tls_socket >>= fun () ->
  wrap_io Lwt_io.close inchan >>= fun () ->
  wrap_io Lwt_io.close outchan >>= fun () ->
  return ()

type client_check_result =
[ `expired of string * Core.Std.Time.t
| `not_found of string
| `revoked of string * Core.Std.Time.t
| `valid of string ]

type client_kind =
[ `anonymous_client
| `invalid_client of
    [ `expired of string * Core.Std.Time.t
    | `not_found of string
    | `revoked of string * Core.Std.Time.t
    | `wrong_certificate ]
| `valid_client of string ]

let plain_server ?on_error ~port f =
  Tls.accept_loop ~port ?on_error
    (fun socket_fd _ ->
      let inchan = Lwt_ssl.in_channel_of_descr  socket_fd in
      let outchan = Lwt_ssl.out_channel_of_descr socket_fd in
      f (connection inchan outchan socket_fd))

let tls_server ?on_error ~port ~cert_key f =
  Tls.server_context cert_key
  >>= fun tls_context ->
  Tls.accept_loop ~tls_context ?on_error ~port
    (fun socket_fd client_kind ->
      let inchan = Lwt_ssl.in_channel_of_descr  socket_fd in
      let outchan = Lwt_ssl.out_channel_of_descr socket_fd in
      f (connection inchan outchan socket_fd))


let authenticating_tls_server
    ~ca_certificate ~check_client_certificate
    ?on_error ~port ~cert_key f =
  Tls.server_context ~ca_certificate cert_key
  >>= fun tls_context ->
  Tls.accept_loop ?on_error ~check_client_certificate ~tls_context ~port
    (fun socket_fd client_kind ->
      let inchan = Lwt_ssl.in_channel_of_descr  socket_fd in
      let outchan = Lwt_ssl.out_channel_of_descr socket_fd in
      f (connection inchan outchan socket_fd) client_kind)


type connection_specification = [
| `tls of
    [ `anonymous | `with_certificate of string * string ]
  * [ `verify_server | `allow_self_signed ]
| `plain
]

let unix_connect sockaddr =
  let socket =
    Lwt_unix.(
      try
        let fd = socket PF_INET SOCK_STREAM 0 in
        fd
      with
      | Unix.Unix_error (e, s, a) as ex ->
        eprintf "Unix.Unix_error: %s %s %s\n%!" (Unix.error_message e) s a;
        raise ex
    ) in
  wrap_io (Lwt_unix.connect socket) sockaddr
  >>= fun () ->
  return socket

let connect ~address specification =
  begin match specification with
  | `plain ->
    unix_connect address >>= fun unix_socket_fd ->
    let socket_fd = Lwt_ssl.plain unix_socket_fd in
    let inchan = Lwt_ssl.in_channel_of_descr  socket_fd in
    let outchan = Lwt_ssl.out_channel_of_descr socket_fd in
    return (connection inchan outchan socket_fd)
  | `tls (connection_type, verification_policy) ->
    unix_connect address >>= fun unix_socket_fd ->
    Tls.client_context ~verification_policy connection_type >>= fun tls_context ->
    Tls.tls_connect unix_socket_fd tls_context >>= fun socket_fd ->
    let inchan = Lwt_ssl.in_channel_of_descr  socket_fd in
    let outchan = Lwt_ssl.out_channel_of_descr socket_fd in
    return (connection inchan outchan socket_fd)
  end



