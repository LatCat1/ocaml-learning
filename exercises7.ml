(* promise and resolve *)

(** A signature for Lwt-style promises, with better names. *)
module type PROMISE = sig
  type 'a state =
    | Pending
    | Fulfilled of 'a
    | Rejected of exn

  type 'a promise

  type 'a resolver

  (** [make ()] is a new promise and resolver. The promise is pending. *)
  val make : unit -> 'a promise * 'a resolver

  (** [return x] is a new promise that is already fulfilled with value
      [x]. *)
  val return : 'a -> 'a promise

  (** [state p] is the state of the promise. *)
  val state : 'a promise -> 'a state

  (** [fulfill r x] resolves the promise [p] associated with [r] with
      value [x], meaning that [state p] will become [Fulfilled x].
      Requires: [p] is pending. *)
  val fulfill : 'a resolver -> 'a -> unit

  (** [reject r x] rejects the promise [p] associated with [r] with
      exception [x], meaning that [state p] will become [Rejected x].
      Requires: [p] is pending. *)
  val reject : 'a resolver -> exn -> unit

  (** [bind p c] registers callback [c] with promise [p].
      When the promise is fulfilled, the callback will be run
      on the promises's contents.  If the promise is never
      fulfilled, the callback will never run. *)
  val bind : 'a promise -> ('a -> 'b promise) -> 'b promise

  val map : ('a -> 'b) -> 'a promise -> 'b promise
end

module Promise : PROMISE = struct
  type 'a state = Pending | Fulfilled of 'a | Rejected of exn

  (** RI: the input may not be [Pending]. *)
  type 'a handler = 'a state -> unit

  (** RI: if [state <> Pending] then [handlers = []]. *)
  type 'a promise = {
    mutable state : 'a state;
    mutable handlers : 'a handler list
  }

  let enqueue
      (handler : 'a state -> unit)
      (promise : 'a promise) : unit
    =
    promise.handlers <- handler :: promise.handlers

  type 'a resolver = 'a promise

  (** [write_once p s] changes the state of [p] to be [s].  If [p] and [s]
      are both pending, that has no effect.
      Raises: [Invalid_arg] if the state of [p] is not pending. *)
  let write_once p s =
    if p.state = Pending
    then p.state <- s
    else invalid_arg "cannot write twice"

  let make () =
    let p = {state = Pending; handlers = []} in
    p, p

  let return x =
    {state = Fulfilled x; handlers = []}

  let state p = p.state

  (** Requires: [st] may not be [Pending]. *)
  let resolve (r : 'a resolver) (st : 'a state) =
    assert (st <> Pending);
    let handlers = r.handlers in
    r.handlers <- [];
    write_once r st;
    List.iter (fun f -> f st) handlers

  let reject r x =
    resolve r (Rejected x)

  let fulfill r x =
    resolve r (Fulfilled x)

  let fail exc = {state = Rejected exc; handlers = []}

  let copying_handler (resolver : 'a resolver) : 'a handler
    = function
      | Pending -> failwith "handler RI violated"
      | Rejected exc -> reject resolver exc
      | Fulfilled x -> fulfill resolver x

  let handler_of_callback
      (callback : 'a -> 'b promise)
      (resolver : 'b resolver) : 'a handler =
      fun (state : 'a state) ->
      match state with
        | Pending -> failwith "handler RI violated"
        | Rejected exc -> reject resolver exc
        | Fulfilled x ->
          try
            let promise = callback x in
            match promise.state with
            | Fulfilled y -> fulfill resolver y
            | Rejected exc -> reject resolver exc
            | Pending -> enqueue (copying_handler resolver) promise
          with exc -> reject resolver exc

  let bind
      (input_promise : 'a promise)
      (callback : 'a -> 'b promise) : 'b promise
    =
    match input_promise.state with
    | Fulfilled x -> (try callback x with exc -> fail exc)
    | Rejected exc -> fail exc
    | Pending ->
      let output_promise, output_resolver = make () in
      enqueue (handler_of_callback callback output_resolver) input_promise;
      output_promise

  let map
      (f: 'a -> 'b)
      (input_promise : 'a promise) : 'b promise
    =
    match input_promise.state with
    | Fulfilled x -> (try return @@ f x with exc -> fail exc)
    | Rejected exc -> fail exc
    | Pending -> 
        let output_promise, output_resolver = make () in
        enqueue (handler_of_callback (Fun.compose return f) output_resolver) input_promise;
        output_promise
end

let int_promise, int_resolver = Promise.make ()
let bound_promise = Promise.map print_int int_promise

(* promise and resolve lwt *)

(*
let lwt_int_promise, lwt_int_resolver = Lwt.wait ()
let lwt_bound_promise = Lwt.bind lwt_int_promise (Fun.compose Lwt_io.printl string_of_int)
*)

(* map via bind *)
(*
  let map
      (callback : 'a -> 'b)
      (input_promise : 'a promise) : 'b promise
    = bind input_promise @@ Fun.compose return callback
*)

(* map anew *)
(* see above *)

(* timing challenge 1 *)
(** [delay s] is a promise that resulves after about [s] seconds. *)

(*
let delay (sec : float) : unit Lwt.t =
  Lwt_unix.sleep sec


let delay_then_print () : unit Lwt.t =
  let* () = delay 3. in
  Lwt_io.printl "done"
*)

(* timing challenge 2 *)

(* prints 1 after 1 second,
   prints 2 after 10 more,
   prints 3 after 20 secons,
   then immediately prints "all done".
   it finishes in 35s overall *)

(*
let delay n = Lwt_unix.sleep n

let timing2 () =
  let* () = delay 1. in
  let* () = Lwt_io.printl "1" in
  let* () = delay 10. in
  let* () = Lwt_io.printl "2" in
  let* () = delay 20. in
  let* () = Lwt_io.printl "3" in
  Lwt_io.printl "all done"

let _ = timing2 ()
let _ = Lwt_main.run (delay 35.)
*)

(* timing challenge 3 *)
(* same as before *)

(*
open Lwt.Syntax

let delay n = Lwt_unix.sleep n

let timing3 () =
  let _t1 = let* () = delay 1. in Lwt_io.printl "1" in
  let _t2 = let* () = delay 10. in Lwt_io.printl "2" in
  let _t3 = let* () = delay 20. in Lwt_io.printl "3" in
  Lwt_io.printl "all done"

let _ = timing3 ()
let _ = Lwt_main.run (delay 35.)
*)

(* timing challenge 4 *)
(* same as before again *)
(*
open Lwt.Syntax

let delay n = Lwt_unix.sleep n

let timing4 () =
  let t1 = let* () = delay 1. in Lwt_io.printl "1" in
  let t2 = let* () = delay 10. in Lwt_io.printl "2" in
  let t3 = let* () = delay 20. in Lwt_io.printl "3" in
  let* () = Lwt.join [t1; t2; t3] in
  Lwt_io.printl "all done"

let _ = timing4 ()
let _ = Lwt_main.run (delay 35.)
*)

(* file moniter *)

(*
(** [loop ic] reads one line from [ic], prints it to stdout,
    then calls itself recursively. It is an infinite loop. *)
let rec loop (ic : input_channel) =
  let* line = read_line ic in
  let* ()   = printl line in
  loop ic

(** [handler] is a helper function for [main]. If its input is
    [End_of_file], it handles cleanly exiting the program by
    returning the unit promise. Any other input is re-raised
    with [Lwt.fail]. *)
let handler : exn -> unit Lwt.t = function
  | End_of_file -> Lwt.return ()
  | exn -> Lwt.fail exn
*)

(* add opt *)
module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Maybe : Monad = struct
  type 'a t = 'a option
  let return x = Some x
  let ( >>= ) m f = match m with
    | None -> None
    | Some x -> f x
end

let add x y : int Maybe.t = Maybe.(
  x >>= fun a ->
  y >>= fun b ->
    return (a + b))

(* fmap and join *)
module type ExtMonad = sig
  type 'a t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val join : 'a t t -> 'a t
end

module ExtMaybe1 : ExtMonad = struct
  type 'a t = 'a option
  let return x = Some x
  let ( >>= ) m f = match m with
    | Some x -> f x
    | None -> None
  let ( >>| ) m f = match m with
    | Some x -> Some (f x)
    | None -> None
  let join m = match m with
    | Some x -> x
    | None -> None
end

(* fmap and join again *)
module ExtMaybe2 : ExtMonad = struct
  type 'a t = 'a option
  let return x = Some x
  let ( >>= ) m f = match m with
    | Some x -> f x
    | None -> None
  let ( >>| ) m f = m >>= (Fun.compose return f)
  let join m = m >>= Fun.id
end

(* bind from fmap+join *)
module type FmapJoinMonad = sig
  type 'a t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val join : 'a t t -> 'a t
  val return : 'a -> 'a t
end

module type BindMonad = sig
  type 'a t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module MakeMonad (M : FmapJoinMonad) : BindMonad = struct
  type 'a t = 'a M.t
  let return = M.return
  let ( >>= ) m f = 
    M.join M.(m >>| f)
end

(* List Monad *)
module ListMonad : ExtMonad = struct
  type 'a t = 'a list
  let return = List.singleton
  (* Flip doesn't work here for some reason
     to do with binding weak variables *)
  let ( >>= ) x y = List.concat_map y x
  let ( >>| ) x y = List.map y x
  let join = List.concat
end

(* trivial monad laws *)
module Trivial : Monad = struct
  type 'a t = Wrap of 'a
  let return x = Wrap x
  let ( >>= ) (Wrap x) f = f x
end

(* The three monad laws:
  1. return a >>= h === ( >>= ) (Wrap a) h === h a
  2. m >>= return === ( >>= ) (Wrap x) return === return x === m
  3. (m >>= g) >>= h === (>>=) ((>>=) (Wrap x) g) h
                     === (>>=) (g x) h
                     where g x = Wrap y
                     === h y
     m >>= (\a -> g a >>= h) === (>>=) (Wrap x) (\a -> g a >>= h)
                             === g x >>= h
                             === h y
*)
