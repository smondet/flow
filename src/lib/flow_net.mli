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

(** High-level TCP + TLS connection handling. *)

open Flow_base

(** {3 TLS Initialization} *)

val init_tls :  unit -> unit
(** Initialize the SSL library. *)

(** {3 Generic Connection Handle} *)

(** A connection is full duplex and can be shut down. *)
type connection
val in_channel: connection -> Lwt_io.input_channel
val out_channel: connection -> Lwt_io.output_channel

(**  Note that [shutdown connection] closes the channels. *)
val shutdown : connection -> (unit, [> `net_exn of exn ]) Flow_base.t

(** {3 Client Connection} *)

type connection_specification = [
| `tls of
    [ `anonymous | `with_certificate of string * string ]
  * [ `verify_server | `allow_self_signed ]
| `plain
]
(** Specification of the kind of connection (for the function [connect]). *)

val connect: address:Lwt_unix.sockaddr -> connection_specification ->
  (connection, [> `net_exn of exn | `tls_context_exn of exn ]) Flow_base.t
(** Connect to the server at [address]. *)

(** {3 Server Establishment} *)

val plain_server :
  ?on_error:(([> `accept_exn of Core.Std.Exn.t
              | `net_exn of exn
              | `not_an_ssl_socket
              | `tls_accept_error of exn ]
                 as 'errors) ->
             (unit, [> `net_exn of exn ]) Flow_base.t) ->
  port:int ->
  (connection -> (unit, 'errors) Flow_base.t) ->
  (unit, [> `net_exn of exn | `socket_creation_exn of exn ]) Flow_base.t
(** Start a “plain” TCP server on port [port]. This function returns
    immediately, the “accept-loop” runs in {i Lwt} threads.

    The function [on_error] is an error handler, it will be called on
    "acceptation" errors (c.f.
    {{:http://ocsigen.org/lwt/api/Lwt_unix#VALaccept_n}Lwt_unix.accept_n})
    {b and} on the remaining errors of the handler (but errors of the
    [on_error] function itself will be ignored).

    Example: {[
    plain_server ~port:4242
      ~on_error:(fun e -> logf "Error: %s" (string_of_error e))
      (fun connection ->
         Flow_base_io.bin_send connection#out_channel "Hello !!"
         >>= fun () ->
         connection#shutdown)
    ]}
*)

val tls_server :
  ?on_error:(([> `accept_exn of Core.Std.Exn.t
              | `net_exn of exn
              | `not_an_ssl_socket
              | `tls_accept_error of exn ]
                 as 'a) ->
             (unit, [> `net_exn of exn ]) Flow_base.t) ->
  port:int ->
  cert_key:string * string ->
  (connection -> (unit, 'a) Flow_base.t) ->
  (unit, [> `socket_creation_exn of exn | `tls_context_exn of exn ])
           Flow_base.t
(** Like [plain_server] but with a TLS layer, the server will be
    authenticated with [~cert_key:("cert.crt", "k.key")]. *)

type client_check_result =
[ `expired of string * Core.Std.Time.t
| `not_found of string
| `revoked of string * Core.Std.Time.t
| `valid of string ]
(** The result type expected from [check_client_certificate] functions. *)

type client_kind =
[ `anonymous_client
| `invalid_client of
    [ `expired of string * Core.Std.Time.t
    | `not_found of string
    | `revoked of string * Core.Std.Time.t
    | `wrong_certificate ]
| `valid_client of string ]
(** The different kinds of clients that a authenticating TLS server
    handler has to treat separately.

    In the current implementation, the case [`anonymous_client] is
    never used, clients without certificate are seen as
    [`invalid_client `wrong_certificate].
*)

val authenticating_tls_server :
  ca_certificate:string ->
  check_client_certificate:(Ssl.certificate ->
                            (client_check_result,
                             [> `accept_exn of Core.Std.Exn.t
                             | `net_exn of exn
                             | `not_an_ssl_socket
                             | `tls_accept_error of exn ]
                               as 'a)
                              Flow_base.t) ->
  ?on_error:('a -> (unit, [> `net_exn of exn ]) Flow_base.t) ->
  port:int ->
  cert_key:string * string ->
  (connection -> client_kind ->
   (unit, 'a) Flow_base.t) ->
  (unit, [> `socket_creation_exn of exn | `tls_context_exn of exn ])
    Flow_base.t
(** Start an authenticating TLS server,  the [ca_certificate] is used to check
    client certificates, and the function [check_client_certificate] to
    decide about the CA-validity of the certificate once it has been validated
    with respect to the TLS protocol. *)

(** {3 Send and Receive Binary Messages } *)

(** The maximum message length (10 million bytes right now). *)
val max_message_length: int

(** Write a message on an output channel (size: 32 bits big endian, string). *)
val bin_send : Lwt_io.output_channel -> string ->
  (unit,
   [> `bin_send of [> `exn of exn | `message_too_long of string ] ]) t

(** Read a message sent by [bin_send]. *)
val bin_recv : Lwt_io.input_channel ->
  (string,
   [> `bin_recv of [> `exn of exn | `wrong_length of int * string ] ]) t

