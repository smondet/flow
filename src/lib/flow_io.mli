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

    
(** {3 Biocaml/Crytokit-style transforms} *)

module Transform: sig

  class type ['input, 'output] t = object
    method next: [ `output of 'output | `end_of_stream | `not_ready ]
    method feed:  'input -> unit
    method stop: unit
  end
  (** Universal (buffered) transformation between streams. *)

  val transform_stream: ('a, 'b) t -> 'a Lwt_stream.t -> 'b Lwt_stream.t
  (** Use a transform to process a [Lwt_stream.t]. *) 
          
  val file_to_file:
    (string, (string, 'error) Core.Result.t) t ->
    ?input_buffer_size:int ->
    string ->
    ?output_buffer_size:int ->
    string ->
    (unit, [> `transform of [> `io_exn of exn
                            | `transform_error of 'error
                            | `stopped_before_end_of_stream]]) Flow_base.t
(** Run a transform between two files. The default values for the
    buffer sizes are 42000, one should choose the ones that fit their
    file-system. *)
end
