The Flow Monad
==============

Exceptionless “systems” library on top of Core and Lwt.

Build:
======

Just run:
```
omake && omake install
```
(you can control the destination with the standard 'OCAMLFIND_DESTDIR'
variable).

Quick Example:
=============

The idea is to get the type-checker tell you everything that may go
wrong, and also to force you to treat all cases:

```ocaml
#use "topfind"
#thread
#require "flow"
open Core.Std

let read_some_file_with_timeout () =
  Flow.Sys.with_timeout 4.2 ~f:(fun () ->
    Flow.IO.read_file "/etc/passwd")
```

The possible errors of `with_timeout` and `read_file` will be merged
and automatically documented:

```ocaml
val read_some_file_with_timeout :
  unit ->
  (string,
   [> `io_exn of exn | `read_file_error of string * exn | `timeout of float ])
  Flow_base.t = <fun>
```

You cannot forget one case:

```ocaml
let print_what_happens =
  let open Flow in
  read_some_file_with_timeout ()
  >>< begin function
  | Ok () -> say "It's OK!\n"
  | Error (`io_exn e) ->
    say "I/O Error: %s\n" (Exn.to_string e)
  | Error (`timeout f) ->
    say "It timeout-ed (%f) !\n" f
  end
```

OCaml says:

```
Error: This pattern matches values of type
         (unit, [< `io_exn of 'a | `timeout of 'b ]) Core.Result.t
       but a pattern was expected which matches values of type
         (string,
          [> `io_exn of exn
           | `read_file_error of string * exn
           | `timeout of float ])
         Core.Result.t
```
  
