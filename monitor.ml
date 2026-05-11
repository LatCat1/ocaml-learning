open Lwt_io
open Lwt_unix
open Lwt.Syntax

(** [log ()] is a promise for an [input_channel] that reads from
    the file named "log". *)
let log () : input_channel Lwt.t =
  let* fd = openfile "log" [O_RDONLY] 0 in
  Lwt.return (of_fd ~mode:input fd)

(** [loop ic] reads one line from [ic], prints it to stdout,
    then calls itself recursively. It is an infinite loop. *)
let rec loop (ic : input_channel) =
  let* line = read_line ic in
  let* ()   = printl line in
  loop ic

(** [monitor ()] monitors the file named "log". *)
let monitor () : unit Lwt.t =
  Lwt.bind (log ()) loop

(** [handler] is a helper function for [main]. If its input is
    [End_of_file], it handles cleanly exiting the program by
    returning the unit promise. Any other input is re-raised
    with [Lwt.fail]. *)
let handler : exn -> unit Lwt.t = function
  | End_of_file -> Lwt.return ()
  | exn -> Lwt.fail exn

let main () : unit Lwt.t =
  Lwt.catch monitor handler

let _ = Lwt_main.run (main ())
