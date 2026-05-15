(* hash insert *)

(*
Table:
  0: [ 42 ]
  1: [ 8 ; 15 ]
  2: [ 16 ; 23 ]
  3: []
  4: [ 4 ]
  5: []
  6: []
*)

(* relax bucket ri *)
(*
The efficiency of insert would not change.
The efficience of find and remove would get slower
*)

(* strengthen bucket ri *)
(*
The efficiency of insert would slow down
The efficiency of find and remove would be faster
*)

(* hash values *)
(* hashtbl usage *)
(* hashtbl stats *)

(* done *)

(* hashtbl bindings *)

let bindings (tab : ('a, 'b) Hashtbl.t) : ('a * 'b) list =
  Hashtbl.fold (fun a b acc -> (a, b) :: acc) tab []

(* hashtbl load factor *)
let load_factor (tab : ('a, 'b) Hashtbl.t) : float =
  let stats = Hashtbl.stats tab in
  (float_of_int stats.num_bindings) /. (float_of_int stats.num_buckets)

(* functional interface *)

module UncapsString : Hashtbl.HashedType = struct
  type t = string
  let equal s1 s2 = String.lowercase_ascii s1 = String.lowercase_ascii s2
  let hash = Fun.compose Hashtbl.hash String.lowercase_ascii
end

module UncapsStringHashtbl = Hashtbl.Make(UncapsString) 

(* equals and hash *)

(* 
You need a hash function (duh) and also a equals function
to compare in case of a hash collision
*)

(* bad hash *)

module BadIntHash : Hashtbl.HashedType = struct
  type t = Int.t
  let equal = (=)
  let hash = Fun.const 0
end

module BadIntHashtbl = Hashtbl.Make(BadIntHash)

(* linear probing *)

module type HashType = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end

type hashtablestats = {
  used_load_factor: float;
  filled_load_factor: float
}

module type HashTable = sig
  type key
  type 'value t

  (** [find tab k] looks for k in the table. raises [Not_found]
      if it is not present *)
  val find : 'value t -> key -> 'value

  (** [insert tab k v] inserts the pair (k, v) in the table, overwriting any exisitng
      'value for k if there is one *)
  val insert : 'value t -> key -> 'value -> unit

  (** [remove tab k] deletes key k from the table *)
  val delete : 'value t -> key -> unit

  (** [stats tab] is the current statistics of the table *)
  val stats : 'value t -> hashtablestats

  (** [empty ()] creates an empty HashTable *)
  val empty : unit -> 'value t
end

module LinearProbingHashTable (T : HashType) : HashTable with type key = T.t = struct

  type ('key, 'value) cell = 
    | Empty 
    | Deleted 
    | Value of ('key * 'value)

  let is_value = function
    | Empty -> true
    | _ -> false

  type key = T.t
  type 'value t = {
    mutable deleted : int;
    mutable filled : int;
    mutable array : (key, 'value) cell array
  }

  let empty () = {deleted=0; filled=0; array=Array.make 4 Empty}

  let stats table = {
    used_load_factor = float_of_int (table.deleted + table.filled) /. float_of_int (Array.length table.array);
    filled_load_factor = float_of_int table.filled /. float_of_int (Array.length table.array)
  }

  (* this assumes that a "Empty" or "Delete" _will_ always be found *)
  let probe_insert array key value = 
    let rec probe_insert_r i = match array.(i) with
      | Empty -> array.(i) <- Value (key, value)
      | Deleted -> array.(i) <- Value (key, value)
      | Value (key', _) when T.equal key key' -> array.(i) <- Value (key, value)
      | _ -> probe_insert_r ((i + 1) mod Array.length array)
    in
    probe_insert_r (T.hash key mod Array.length array)

  let resize_if_needed table = 
    let tablestats = stats table in
    match () with
    | _ when tablestats.used_load_factor > 0.5 || tablestats.filled_load_factor < 0.125 -> 
      let new_array = Array.make (Array.length table.array * 2) Empty in
      let filled_count = Array.fold_left (Fun.flip @@ Fun.compose (+) (Fun.compose Bool.to_int is_value)) 0 table.array in
      Array.map (function
        | Empty -> ()
        | Deleted -> ()
        | Value (k, v) -> probe_insert new_array k v
      ) table.array
      |> ignore ;
      table.deleted <- 0;
      table.filled <- filled_count ;
      table.array <- new_array ;
    | _ -> ()

  let find tab k =
    let len = Array.length tab.array in
    let rec find_r i = match tab.array.(i) with
      | Empty -> raise Not_found
      | Value (key', value) when T.equal k key' -> value
      | _ -> find_r ((i + 1) mod len) in
    find_r (T.hash k mod len)

  let insert tab k v =
    probe_insert tab.array k v ;
    resize_if_needed tab

  let delete tab k = 
    let len = Array.length tab.array in
    let rec delete_r i = match tab.array.(i) with
      | Empty -> raise Not_found
      | Deleted -> delete_r ((i + 1) mod len)
      | Value (key', _) when T.equal k key' -> tab.array.(i) <- Deleted
      | Value _ -> delete_r ((i + 1) mod len)
    in
    delete_r (T.hash k mod len) ;
    resize_if_needed tab
end

module IntHashType : HashType with type t = int = struct
  type t = int
  let equal = (=)
  let hash = Fun.id
end

(* functorized BST *)

type comparison =
  | Less
  | Equal
  | Greater

module type Comparable = sig
  type t
  (** [compare x y] is -1 when x < y, 0 if x = y, and 1 if x > y *)
  val compare : t -> t -> comparison
end

module type MySet = sig
  type item
  type set

  (** [add x set] is a new set with x added, even if already present *)
  val add : item -> set -> set

  (** [remove x set] is a new set with x removed if present, otherwise throwing Not_found *)
  val remove : item -> set -> set

  (** [mem x set] is true if x is present, and false otherwise *)
  val mem : item -> set -> bool

  (** [empty] is an empty set *)
  val empty : set
end

module BstSet (Comp : Comparable) : MySet with type item = Comp.t = struct
  (* Not bothering to implement any balancing/pivoting *)
  type item = Comp.t
  type set =
    (* | Node of {left : set; val : item; right : set} Can't name them? *) 
    | Node of set * item * set
    | Empty

  let empty = Empty

  let rec add item = function
    | Empty -> Node (Empty, item, Empty)
    | Node (l, item', r) -> match Comp.compare item item' with
      | Less -> Node (add item l, item', r)
      | Equal -> Node (l, item, r)
      | Greater -> Node(l, item', add item r)

  let rec remove item = function
    | Empty -> raise Not_found
    | Node (l, item', r) -> match Comp.compare item item' with
      | Less -> Node (remove item l, item', r)
      | Equal -> (match (l, r) with
        | (Empty, Empty) -> Empty
        | (Node (l', i'', r'), Empty) -> Node (l', i'', r')
        | (Empty, Node (l', i'', r')) -> Node (l', i'', r')
      (* Not implemented because it requires going another level down *)
        | _ -> raise Not_found
      )
      | Greater -> Node (l, item', remove item r)

  let rec mem item = function
    | Empty -> false
    | Node (l, item', r) -> match Comp.compare item item' with
      | Less -> mem item l
      | Equal -> true
      | Greater -> mem item r
end

module type BootstrapComparableType = sig
  type t
  val compare : t -> t -> int
end

module BootstrapComparable (Base : BootstrapComparableType) : Comparable with type t = Base.t = struct
  type t = Base.t
  let compare x y = match Base.compare x y with
    | v when v < 0 -> Less
    | v when v = 0 -> Equal
    | v when v > 0 -> Greater
    (* Should never be reached *)
    | _ -> raise Not_found
end

(** efficient traversals *)
 
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let preorder (t : 'a tree) : 'a list =
  let rec preorder_accum (tree : 'a tree) (list : 'a list) : 'a list = match tree with
    | Leaf -> list
    | Node (l, v, r) -> list
      |> preorder_accum r
      |> preorder_accum l
      |> (fun list' -> v :: list') 
  in preorder_accum t []

let inorder (t : 'a tree) : 'a list =
  let rec inorder_accum (tree : 'a tree) (list : 'a list) : 'a list = match tree with
    | Leaf -> list
    | Node (l, v, r) -> list
      |> inorder_accum r
      |> (fun list' -> v :: list') 
      |> inorder_accum l
  in inorder_accum t []

let postorder (t : 'a tree) : 'a list =
  let rec postorder_accum (tree : 'a tree) (list : 'a list) : 'a list = match tree with
    | Leaf -> list
    | Node (l, v, r) -> list
      |> (fun list' -> v :: list') 
      |> postorder_accum r
      |> postorder_accum l
  in postorder_accum t []


(* skipping red/black trees *)

(* pow2 *)

type 'a sequence = Cons of 'a * (unit -> 'a sequence)

let pow2 : int sequence =
  let rec helper i () =
    Cons (i, helper (2*i))
  in helper 1 ()

let rec seq_to_list (Cons (x, next) : 'a sequence) : int -> 'a list = function
  | 0 -> []
  | n -> x :: seq_to_list (next ()) (n - 1)

let print_seq (seq : 'a sequence) (n : int) (printer : 'a -> string) =
  "<" ^ String.concat ";" (List.map printer @@ seq_to_list seq n) ^ ">"

(* more sequences *)
let evens : int sequence =
  let rec helper i () =
    Cons (i, helper (i+2))
  in helper 1 ()

let loweralpha : char sequence =
  let next_letter c =
    Char.code c |> Fun.flip (-) (Char.code 'a') |> (+) 1 |> (Fun.flip (mod) 26) |> (+) (Char.code 'a') |> Char.chr
  in let rec helper c () =
    Cons (c, helper @@ next_letter c)
  in helper 'a' ()

let flips : bool sequence =
  let rec helper () =
    Cons (Random.bool (), helper)
  in helper ()

(* nth *)

let nats : int sequence =
  let rec helper i () =
    Cons (i, helper (i+1))
  in helper 0 ()

let hd (Cons (x, _)) = x
let tl (Cons (_, next)) = next ()

let rec nth (Cons (x, next)) = function
  | 0 -> x
  | n -> nth (next ()) n - 1

(* head tl *)

(*... duh *)

(* filter *)

let rec filter p (Cons (x, next)) =
  if p x
  then Cons (x, Fun.compose (filter p) next)
  else filter p @@ next ()


(* interleave *)

let rec interleave (Cons (x, next_x)) ys =
  Cons (x, Fun.compose (interleave ys) next_x)

(* sift *)

let sift n = filter (fun x -> x mod n != 0)

(* primes *)

let prime : int sequence =
  let start_with_2 = tl @@ tl nats in
  let rec helper (Cons (p, next)) () =
    Cons (p, helper (sift p @@ next ())) 
  in
  helper start_with_2 ()

(* approximately e *)

let rec seq_map f (Cons (x, next_x)) =
  Cons (f x, Fun.compose (seq_map f) next_x)

let rec seq_accum f acc (Cons (x, next_x)) =
  let acc' = f acc x in
  Cons (acc', Fun.compose (seq_accum f acc') next_x)

let rec seq_zip (Cons (x, next_x)) (Cons (y, next_y)) =
  Cons ((x, y), fun u -> seq_zip (next_x u) (next_y u))

let rec cons x seq = Cons (x, fun () -> seq)

let e_terms x = seq_map (fun (i, fact_i) -> (x ** (float_of_int i)) /. float_of_int (fact_i)) @@
  seq_zip nats @@ cons 1 @@ seq_accum ( * ) 1 @@ tl nats

let total_float_seq = seq_accum (+.) 0.

(** precondition: within > 0 *)
let within delta seq = snd @@ hd @@ filter (fun (x, y) -> abs_float (x -. y) < delta) @@ seq_zip seq (tl seq)

let e x delta = within delta @@ total_float_seq @@ e_terms x

(* better e *)

let const x = 
  let rec helper () = Cons (x, helper)  in 
  helper ()

let better_e_terms x = seq_accum (fun acc (x, i) -> acc *. x /. (float_of_int i)) 1.
  @@ seq_zip (cons 1. @@ const x) @@ cons 1 @@ tl nats

let better_e x delta = within delta @@ total_float_seq @@ better_e_terms x

(* alternative sequence *)
type 'a altseq = AltCons of (unit -> 'a * 'a altseq)

let althd (AltCons next) = fst @@ next ()
let alttl (AltCons next) = snd @@ next ()

let nats = 
  let rec helper i () : 'a * 'a altseq = (i, helper (i+1)) in
  helper 0
