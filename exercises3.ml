(* List Expressions * *)

let one_through_five: int list = [1; 2; 3; 4; 5]

let one_through_five_two: int list = 1 :: 2 :: 3 :: 4 :: 5 :: [] 

let one_through_five_three: int list = [1] @ [2; 3; 4] @ [5] 

(* Product *)

let rec product = function
    | [] -> 1
    | x :: xs -> x * product xs

(* concat *)

let rec concat = function
    | []      -> ""
    | x :: xs -> x ^ concat xs

(* patterns *)

let bigredcheck : string list -> bool = function
    | "bigred" :: _ -> true
    | _             -> false

let twoorfour : 'a list -> bool = function
    | _ :: _ :: [] -> true
    | _ :: _ :: _ :: _ :: [] -> true
    | _ -> false

let firsttwoequal : 'a list -> bool = function
    | a :: b :: _ -> a = b
    | _ -> false

(* list library *)

open Stdlib

let fifth (l: int list) : int = 
    if (List.length l >= 5) then (List.nth l 4) else 0

let reversesorted (l: int list) : int list =
    List.sort (Stdlib.compare) l |> List.rev

(* library puzzle *)

(* assumes nonempty *)
let lastelement (l: 'a list): 'a =
    List.nth l (List.length l - 1)

let any_zeros (l : int list) : bool =
    let accum_func acc a = acc || (a = 0)
    in List.fold_left accum_func false l

(* take drop *)
let rec take: int -> 'a list -> 'a list = function
    | 0 -> (function _ -> [])
    | n -> function
        | [] -> []
        | x :: xs -> x :: take (n-1) xs

let rec drop (n: int) : 'a list -> 'a list = function
    | [] -> []
    | _ :: xs -> if n = 0 then [] else drop (n-1) xs

(* take drop tail recursion *)

let take_tr (n: int) (xs: 'a list): 'a list =
    let rec helper (n: int) (xs: 'a list) (taken: 'a list): 'a list =
        match n with | 0 -> [] | _ -> match xs with
        | [] -> []
        | x :: xs' -> helper (n - 1) xs' (x :: taken)
    in
    helper n xs []
(* it looks like drop already is ; it performs no work after the recurse *)


(* unimodal *)

let is_unimodal (xs: int list): bool = 
    (* only handles the decreasing *)
    let rec decreasing_helper (xs: int list) (curr: int): bool = match xs with
    | [] -> true
    | x :: xs' -> if x <= curr then decreasing_helper xs' x else false
    in
    (* tries to increase, decreases if failse *)
    let rec increasing_helper (xs: int list) (curr: int): bool = match xs with
    | [] -> true
    | x :: xs' -> (if x >= curr then increasing_helper else decreasing_helper) xs' x
    in match xs with
    | [] -> true
    | x :: xs' -> increasing_helper xs' x

(* powerset *)
let rec powerset (xs: 'a list): 'a list list = match xs with
    | [] -> [[]]
    | x :: xs' -> let p = powerset xs'
        in (List.map (fun xs'' -> x :: xs'') p) @ p

(* print list rec *)
let rec print_int_list (xs: int list): unit = match xs with
    | [] -> ()
    | x :: xs -> string_of_int x |> print_endline; print_int_list xs

(* print list iter *)
let print_int_list' (xs: int list): unit =
    List.iter (Fun.compose print_endline string_of_int) xs

(* student *)
type student = {first_name: string; last_name: string; gpa: float}

(* expression with type student *)
(* stu = {first_name = "John"; last_name = "Johnson"; gpa=4. } *)
let student_name (stu: student): string * string =
    (stu.first_name, stu.last_name)

let construct_student (first_name: string) (last_name: string) (gpa: float): student =
    {first_name = first_name; last_name = last_name; gpa = gpa} 

(* pokerecord *)

type poketype = Normal | Fire | Water

type pokemon = {name: string; hp: int; ptype: poketype}

let charizard = {name="Charizard"; hp=78; ptype=Fire}
let squirtle = {name="Squirtle"; hp=44; ptype=Water}

(* safe hd and tl *)
let safe_hd (xs: 'a list): 'a option = match xs with
    | [] -> None
    | x :: _ -> Some x

let safe_tl (xs: 'a list): 'a list option = match xs with
    | [] -> None
    | _ :: xs' -> Some xs'

(* pokefun *)
let max_hp (ps: pokemon list): pokemon option =
    let rec max_hp_helper (ps: pokemon list) (best_so_far: pokemon): pokemon = match ps with
    | [] -> best_so_far
    | p :: ps' -> max_hp_helper ps' (if p.hp > best_so_far.hp then p else best_so_far)
    in match ps with
    | [] -> None
    | p :: ps' -> Some (max_hp_helper ps' p)

(* date before *)
type date = int * int * int

let is_before (date1: date) (date2: date): bool = match date1 with
    | (y1, m1, d1) -> match date2 with
        | (y2, m2, d2) -> y1 < y2 || y1 == y2 && (
            m1 < m2 || m1 == m2 && d1 < d2)

(* earliest date *)
let earliest (dates: date list): date option = match dates with
    | [] -> None
    | d :: ds -> Some (List.fold_left (fun x y -> if is_before x y then x else y) d ds)

(* assoc list *)

type ('a, 'b) assoc_list = ('a * 'b) list

let insert (k: 'a) (v: 'b) (lst: ('a, 'b) assoc_list) = (k, v) :: lst
let rec lookup (k: 'a): ('a, 'b) assoc_list -> 'b option = function
    | [] -> None
    | (k', v) :: t -> if k = k' then Some v else lookup k t

let example_assoc_list: (int, string) assoc_list =
    [] |> insert 1 "one" |> insert 2 "two" |> insert 3 "three"

let one_lookup = lookup 1 example_assoc_list
let four_lookup = lookup 4 example_assoc_list

(* cards *)
type suit = Clubs | Diamonds | Hearts | Spades
type rank = Number of int | Jack | Queen | King | Ace

type card = suit * rank

let ace_of_clubs: card = (Clubs, Ace)
let queen_of_hearts: card = (Hearts, Queen)
let two_of_diamonds: card = (Diamonds, Number 2)
let seven_of_spades: card = (Spades, Number 7)

(* matching *)

(* empty list sufficies for all of them *)

(* quadrant *)
type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x: int) : sign =
    if x > 0 then Pos else if x < 0 then Neg else Zero

let quadrant : int * int -> quad option = fun (x, y) ->
    match (sign x, sign y) with
    | (Pos, Pos) -> Some I
    | (Neg, Pos) -> Some II
    | (Neg, Neg) -> Some III
    | (Pos, Neg) -> Some IV
    | _ -> None

(* quadrant when *)
let quadrant_when : int * int -> quad option = function
    | (x, y) when x > 0 && y > 0 -> Some I
    | (x, y) when x < 0 && y > 0 -> Some II
    | (x, y) when x < 0 && y < 0 -> Some III
    | (x, y) when x > 0 && y < 0 -> Some IV
    | _ -> None

(* depth *)
type 'a tree =
    | Leaf
    | Node of 'a * 'a tree * 'a tree

let rec depth: 'a tree -> int = function
    | Leaf -> 0
    | Node (_, l, r) -> 1 + max (depth l) (depth r)

(* shape *)
let rec same_shape (tree1: 'a tree) (tree2: 'b tree) : bool =
    match (tree1, tree2) with
    | (Leaf, Leaf) -> true
    | (Node (_, l1, r1), Node (_, l2, r2)) -> same_shape l1 r1 && same_shape l2 r2
    | _  -> false

(* list max exn *)
let list_max : int list -> int =
    let rec list_max_helper (acc: int) : int list -> int = function
    | [] -> acc
    | x :: xs -> list_max_helper (max acc x) xs
    in function
    | x :: xs -> list_max_helper x xs
    | [] -> raise @@ Failure "empty"

(* list max exn string *)
let list_max_string : int list -> string =
    let rec list_max_helper (acc: int) : int list -> int = function
    | [] -> acc
    | x :: xs -> list_max_helper (max acc x) xs
    in function
    | x :: xs -> list_max_helper x xs |> string_of_int
    | [] -> "empty"


(* is bst *)

type bst_state = MinMax of int * int | Empty | Failure

let is_bst (tr: ('a * 'b) tree) : bool =
    let rec is_bst_helper : ('a * 'b) tree -> bst_state = function
        | Leaf -> Empty
        | Node ((k, _), l, r) -> match (is_bst_helper l, is_bst_helper r) with
            | (Failure, _) -> Failure
            | (_, Failure) -> Failure
            | (Empty, Empty) -> MinMax (k, k)
            | (MinMax (l_min, l_max), Empty) -> if l_max < k then MinMax (l_min, k) else Failure
            | (Empty, MinMax (r_min, r_max)) -> if k < r_min then MinMax (k, r_max) else Failure
            | (MinMax (l_min, l_max), MinMax (r_min, r_max)) -> 
                    if (l_max < k && k < r_min) then MinMax (l_min, r_max) else Failure
    in match (is_bst_helper tr) with
    | MinMax _ -> true
    | Empty -> true
    | Failure -> false

(* quadrant poly *)
let sign_poly (x: int) = if x > 0 then `Pos else if x < 0 then `Neg else `Zero

let quadrant_poly ((x, y): int * int) =
    match (sign_poly x, sign_poly y) with
    | (`Pos, `Pos) -> Some I
    | (`Neg, `Pos) -> Some II
    | (`Neg, `Neg) -> Some III
    | (`Pos, `Neg) -> Some IV
    | _ -> None
