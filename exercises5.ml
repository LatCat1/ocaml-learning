(* complex synonym *)

module type ComplexSig = sig
    type t = float * float
    val zero : t
    val add : t -> t -> t
end

(* complex encapsulation *)
module Complex : ComplexSig = struct
    type t = float * float
    let zero = (0., 0.)
    let add (r1, i1) (r2, i2) = r1 +. r2, i1 +. i2
end

(*
Change 1: error due to incomplete instantiation
Change 2: error due to incomplete instantiation
Change 3: error due to mismatching types
*)

(* big list queue *)
module type Queue = sig
  (** An ['a t] is a queue whose elements have type ['a]. *)
  type 'a t

  (** Raised if [front] or [dequeue] is applied to the empty queue. *)
  exception Empty

  (** [empty] is the empty queue. *)
  val empty : 'a t

  (** [is_empty q] is whether [q] is empty. *)
  val is_empty : 'a t -> bool

  (** [enqueue x q] is the queue [q] with [x] added to the end. *)
  val enqueue : 'a -> 'a t -> 'a t

  (** [front q] is the element at the front of the queue. Raises [Empty]
      if [q] is empty. *)
  val front : 'a t -> 'a

  (** [dequeue q] is the queue containing all the elements of [q] except the
      front of [q]. Raises [Empty] if [q] is empty. *)
  val dequeue : 'a t -> 'a t

  (** [size q] is the number of elements in [q]. *)
  val size : 'a t -> int

  (** [to_list q] is a list containing the elements of [q] in order from
      front to back. *)
  val to_list : 'a t -> 'a list
end

module ListQueue : Queue = struct
  (** The list [x1; x2; ...; xn] represents the queue with [x1] at its front,
      followed by [x2], ..., followed by [xn]. *)
  type 'a t = 'a list
  exception Empty
  let empty = []
  let is_empty = function [] -> true | _ -> false
  let enqueue x q = q @ [x]
  let front = function [] -> raise Empty | x :: _ -> x
  let dequeue = function [] -> raise Empty | _ :: q -> q
  let size = List.length
  let to_list = Fun.id
end

let fill_listqueue n = 
    let rec loop n q =
        if n = 0 then q
        else loop (n - 1) (ListQueue.enqueue n q) in
    loop n ListQueue.empty

(* This seems to get slow around 10_000 and really delay at 100_000 *)

(* big batched queue *)
module BatchedQueue : Queue = struct
  (** [{o; i}] represents the queue [o @ List.rev i]. For example,
      [{o = [1; 2]; i = [5; 4; 3]}] represents the queue [1, 2, 3, 4, 5],
      where [1] is the front element. To avoid ambiguity about emptiness,
      whenever only one of the lists is empty, it must be [i]. For example,
      [{o = [1]; i = []}] is a legal representation, but [{o = []; i = [1]}]
      is not. This implies that if [o] is empty, [i] must also be empty. *)
  type 'a t = {o : 'a list; i : 'a list}

  exception Empty

  let empty = {o = []; i = []}

  let is_empty = function
    | {o = []; _} -> true
    | _ -> false

  let enqueue x = function
    | {o = []; _} -> {o = [x]; i = []}
    | {o; i} -> {o; i = x :: i}

  let front = function
    | {o = []; _} -> raise Empty
    | {o = h :: _; _} -> h

  let dequeue = function
    | {o = []; _} -> raise Empty
    | {o = [_]; i} -> {o = List.rev i; i = []}
    | {o = _ :: t; i} -> {o = t; i}

  let size {o; i} = List.(length o + length i)

  let to_list {o; i} = o @ List.rev i
end

let fill_batchedqueue n =
  let rec loop n q =
    if n = 0 then q
    else loop (n - 1) (BatchedQueue.enqueue n q) in
  loop n BatchedQueue.empty

(* This one gets slow at a 100 million elements *)

(* queue efficiency *)

(*

enqueue is extremely slow in ListQueue because it needs to
create an entirely new copy of everything, every time

it is much faster for for BatchedQueue because it can stick them
'on the front' of the reversed part of the queue, which is constant time

*)

(* binary search tree map *)
module type Map = sig
  (** [('k, 'v) t] is the type of maps that bind keys of type ['k] to
      values of type ['v]. *)
  type ('k, 'v) t

  (** [empty] does not bind any keys. *)
  val empty  : ('k, 'v) t

  (** [insert k v m] is the map that binds [k] to [v], and also contains
      all the bindings of [m].  If [k] was already bound in [m], that old
      binding is superseded by the binding to [v] in the returned map. *)
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

  (** [lookup k m] is the value bound to [k] in [m]. Raises: [Not_found] if [k]
      is not bound in [m]. *)
  val lookup : 'k -> ('k, 'v) t -> 'v

  (** [bindings m] is an association list containing the same bindings as [m].
      The keys in the list are guaranteed to be unique. *)
  val bindings : ('k, 'v) t -> ('k * 'v) list
end

module BstMap : Map = struct 
    type ('k, 'v) t = | Node of ('k * 'v) * (('k, 'v) t) * (('k, 'v ) t) | Leaf
    let empty = Leaf 

    let rec insert k v = function
        | Leaf -> Node ((k, v), Leaf, Leaf)
        | Node ((k', v'), l, r) ->
                if k = k' then Node ((k, v'), l, r) else
                if k < k' then Node ((k', v'), insert k v l, r) else
                               Node ((k', v'), l, insert k v r)

    let rec lookup k = function
        | Leaf -> raise Not_found
        | Node ((k', v), l, r) -> if k' = k then v else 
            if k < k' then lookup k l else lookup k r

    let rec bindings = function
        | Leaf -> []
        | Node ((k, v), l, r) -> (k, v) :: (bindings l) @ (bindings r)
end

(* fraction *)
(* also fraction reduced *)
module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0. *)
  type t

  (** [make n d] represents n/d, a fraction with 
      numerator [n] and denominator [d].
      Requires d <> 0. *)
  val make : int -> int -> t

  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float

  val add : t -> t -> t
  val mul : t -> t -> t
end

(** [gcd x y] is the greatest common divisor of [x] and [y].
    Requires: [x] and [y] are positive. *)
let rec gcd x y =
  if x = 0 then y
  else if (x < y) then gcd (y - x) x
  else gcd y (x - y)

module IntFraction : Fraction = struct
    (* we always represent in reduced form,
       with a positive denominator *)
    type t = int * int

    (* helper fuction to take an arbitrary form
       fraction and enforce the desired representation on it *)
    let enforce_representation (n, d): t =
        let g = gcd (abs n) (abs d) in
        let s_d = if d > 0 then 1 else -1 in
        n * s_d / g, d * s_d / g 

    let make n d = enforce_representation (n, d)

    let numerator = fst
    let denominator = snd
    let to_string (n, d) = "(" ^ string_of_int n ^ "/" ^ string_of_int d ^ ")"
    let to_float (n, d) = (float_of_int n) /. (float_of_int d)

    let add (n1, d1) (n2, d2) = enforce_representation (n1 * d2 + n2 * d1, d1 * d2)
    let mul (n1, d1) (n2, d2) = enforce_representation (n1 * n2, d1 * d2)
end

(* make char map *)

(*
empty: 'a t = 'a Map.Make(Char).t
This is an empty map, with keys given by chars and implemented
however the Map.Make functor chooses

add: key -> 'a -> 'a t -> 'a t = char -> 'a -> 'a Map.Make(Char).t  -> 'a Map.Make(Char).t
This adds a key/value pair, with keys being given by chars and values being any type

remove: key -> 'a t -> 'a t
This removes a key from the map
*)

(* Char Ordered *)

(*
The Map.OrderedType signature indicates that anything must have a function named compare
'a -> 'a -> int for comparison. Looking at the Char module,  it has a funciton named compare
char -> char -> int; this matches the requirement so we can pass it as an arguement
*)

(* use char map *)
module CharMap = Map.Make(Char)

let created_char_map = 
    CharMap.add 'A' "Alpha" CharMap.empty |>
    CharMap.add 'E' "Echo" |> 
    CharMap.add 'S' "Sierra" |>
    CharMap.add 'V' "Victor"

let e_value = CharMap.find 'E' created_char_map

let removed_a = CharMap.remove 'A' created_char_map
let a_still_bound = CharMap.mem 'A' removed_a
let charmap_bindings = CharMap.bindings removed_a

(* bindings *)

(*

Bindings returns value in increasing order of their key,
so it does not matter the order in which things are added/removed,
just their values

Thus all of the following return the same
*)
let charmap_bindings_1 = CharMap.(empty |> add 'x' 0 |> add 'y' 1 |> bindings)
let charmap_bindings_2 = CharMap.(empty |> add 'y' 1 |> add 'x' 0 |> bindings)
let charmap_bindings_3 = CharMap.(empty |> add 'x' 2 |> add 'y' 1 |> remove 'x' |> add 'x' 0 |> bindings)

(* date order *)

type date = {month : int; day : int }

module Date = struct
    type t = date
    let compare date1 date2 =
        let monthComp = compare date1.month date2.month in
        if monthComp <> 0 then monthComp else compare date1.day date2.day
end

(* calendar *)
module DateMap = Map.Make(Date)

type calendar = string DateMap.t

let some_dates: calendar = DateMap.(
    empty |>
    DateMap.add {month=1; day=1} "New Years" |>
    DateMap.add {month=3; day=14} "Pi Day"
)

(* is for *)
let is_for: string CharMap.t -> string CharMap.t =
    (fun c s -> String.make 1 c ^ " is for " ^ s) |> CharMap.mapi

(* first_after *)
let first_after cal date =
    DateMap.find_first (fun d -> (>) 0 @@ Date.compare date d) cal

(* sets *)

module CaseInsensitiveString = struct
    type t = string
    let compare t1 t2 = 
        compare (String.lowercase_ascii t1) (String.lowercase_ascii t2)
end

module CaseInsensitiveStringSet = Set.Make(CaseInsensitiveString)

(* ToString *)
module type ToString = sig
    type t
    val to_string : t -> string
end

(* Print *)
module Print (M: ToString) = struct
    let print : M.t -> unit = Fun.compose print_string M.to_string
end

(* PrintInt *)

module Int = struct
    type t = int
    let to_string = string_of_int
end

module PrintInt = Print(Int)

(* PrintString *)
module MyString = struct
    type t = string
    let to_string = Fun.id
end

module PrintString = Print(MyString)

(* Print reuse *)

(* 
Our usage of the Print module prevents having to rewrite the
print_string part of the code 
*)

(* Print String reuse revisited *)

module StringWithPrint = struct
    include String
    include Print(MyString) 
end
