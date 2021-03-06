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

(** Useful I/O functions. *)

open Core.Std
open Flow_base


(** {3 Whole Files} *)

(** Write a string to a file. *)
val write_file: string -> content:string ->
  (unit, [> `write_file_error of (string * exn)]) t

(** Read a string from a file. *)
val read_file: string ->
  (string, [> `read_file_error of (string * exn)]) t

(** {3 Access To Channels } *)

(** Run a function [f] on an output channel, if the channel comes from
    a [`file f], it will be closed before returning (in case of success,
    or error, but not for exceptions). *)
val with_out_channel:
  ?buffer_size:int ->
  f:(Lwt_io.output_channel ->
     ('a,
      [> `file_exists of string
      | `io_exn of exn
      | `wrong_path of string ] as 'err) t) ->
  [ `append_to_file of string
  | `create_file of string
  | `overwrite_file of string
  | `channel of Lwt_io.output_channel | `stderr | `stdout] ->
  ('a, 'err) t

(** Safely call [Lwt_io.fprint]. *)
val write: Lwt_io.output_channel -> string -> (unit, [> `io_exn of exn]) t

(** Flush an output channel. *)
val flush: Lwt_io.output_channel ->  (unit, [> `io_exn of exn]) t

(** Run a function [f] on an input channel, if the channel comes from
    a [`file f], it will be closed before returning (in case of success,
    or error, but not for exceptions). *)
val with_in_channel:
  [ `channel of Lwt_io.input_channel | `file of string | `stdin ] ->
  ?buffer_size:int ->
  f:(Lwt_io.input_channel -> ('a, [> `io_exn of exn] as 'err) t) ->
  ('a, 'err) t

(** Read [count] bytes from an input-channel (default: “as much as possible”,
    c.f. {{:http://ocsigen.org/lwt/api/Lwt_io#VALread}Lwt_io.read}). *)
val read: ?count:int -> Lwt_io.input_channel ->
  (string, [> `io_exn of exn]) t

(** {3 Biocaml/Crytokit-style Transforms} *)

module Transform: sig

  class type ['input, 'output] t = object
    method next: [ `output of 'output | `end_of_stream | `not_ready ]
    method feed:  'input -> unit
    method stop: unit
  end
  (** Universal (buffered) transformation between streams. *)

  val to_stream_fun: ('a, 'b) t -> 'a Lwt_stream.t -> 'b Lwt_stream.t
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
