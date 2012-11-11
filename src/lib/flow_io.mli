(** Useful I/O functions. *)

open Core.Std
open Flow_base

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
