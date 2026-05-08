open Stdlib

(* twice, no arguements *)

let double x = 2 * x
let square x = x * x
let twice f x = f @@ f x
let quad = twice double
let fourth = twice square

(*
type of quad: int -> int
type of fourt: int -> it

even though quad is not written as a function, it is typed as
twice double: x -> double (double x) = int and the type on the right is an int
*) 

(* mystery operator 1 *)
let ( $ ) f x = f x

(* 
this is the apply operator! @@ normally. it applies the function, but with low
precidence so any work on the right is done first
*)

(* mystery operator 2 *)
let ( @@ ) f g x = x |> g |> f

(* 
this composes two functions together, applying them
one at a time
*)

(* repeat *)
let rec repeat f n x = 
    if n = 0 then x else repeat f (n - 1) $ f x

(* product *)
let product_left (xs: float list) =
    List.fold_left ( *. ) 1. xs

let product_right (xs: float list) =
    List.fold_right ( *. ) xs 1.

(* terse product *)
let product_left_terse = List.fold_left ( *. ) 1.

let product_right_terse =
    Fun.flip (List.fold_right ( *. )) 1.

(* sum_cube_odd *)
let rec (--) (x: int) (y: int): int list =
    if x <= y then x :: ((x+1) -- y) else []

let sum_cube_odd (n: int): int =
    let cube x = x * x * x in
    let odd x = x mod 2 <> 0 in
    (0 -- n) |> List.filter odd |> List.map cube |> List.fold_left (+) 0

(* sum_cube_odd pipeline *)
(* done above *)

(* exists *)
let rec exists_rec (pred: 'a -> bool): 'a list -> bool = function
    | [] -> false
    (* written this way because I am not sure if OCaml computes th
       second arguement of true || func *)
    | x :: xs -> if pred x then true else exists_rec pred xs

let exists_fold (pred: 'a -> bool): 'a list -> bool =
    List.fold_left (fun acc a -> acc || pred a) false

let exists_lib = List.exists

(* account balance *)

let rec account_balance_rec (balance: int): int list -> int = function
    | [] -> balance
    | d :: ds -> account_balance_rec (balance - d) ds

let account_balance_fold_left : int -> int list -> int =
    List.fold_left (-)

let account_balance_fold_right : int -> int list -> int =
    Fun.flip (-) |> List.fold_right |> Fun.flip

(* library uncurried *)

let uncurried_append (front, back) = List.append front back
let uncurried_compare (c1, c2) = Char.compare c1 c2
let uncurried_max (l, r) = max l r

(* map composition *)

(*
an expression of the form List.map f (List.map g lst)
can be replaced with List.map (f @@ g) lst
*)

(* more list fun *)
let find_strings_longer_than_3: string list -> string list =
    (<) 3 @@ String.length |> List.filter

let inc_floats_by_1: float list -> float list =
    (+.) 1. |> List.map

let string_join (strs: string list) (sep: string) = match strs with
    | [] -> ""
    | s :: ss -> List.fold_left (fun acc s' -> acc ^ sep ^ s') s ss

(* association list keys *)
let keys (assoc_list: ('a * 'b) list) = List.map fst assoc_list |> List.sort_uniq compare

(* valid matrix *)
(* one row, one column, every column has same number of rows *)

let valid_matrix (m: int list list): bool =
    let num_rows = List.length m in
    let num_cols = if num_rows > 0 then List.hd m |> List.length else 0 in
    num_rows > 0 && num_cols > 0 && List.for_all ((=) num_cols @@ List.length) (List.tl m)
(* TODO: unit tests *)

(* row vector add *)
let add_row_vectors = List.map2 (+)

(* matrix add *)
let add_matrices = List.map2 add_row_vectors

(* matrix multiply *)
let dot_product r1 = List.fold_left (+) 0 @@ List.map2 ( * ) r1

let rec transpose : int list list -> int list list = function
    | [] -> []
    (* only empty lists remaining *)
    | [] :: _ -> []
    (* there are things in them *)
    | m -> (List.map List.hd m) :: transpose (List.map List.tl m)

let multiply_matrices m1 =
    transpose @@ List.map (Fun.flip List.map m1 @@ dot_product) @@ transpose
