(* mutable fields *)

type student = {name: string; gpa: float ref }

let alice: student = {name="Alice"; gpa= ref 3.7} ;;

alice.gpa := 4.0 ;;

(* refs *)

let bool_ref: bool ref = ref true
let int_list_ref: int list ref  = ref [1; 2]
let int_ref_list: int ref list = [ref 1; ref 2]

(* inc fun *)
let inc = ref (fun x -> x + 1)
let val_3110 = !inc @@ 3109

(* addition assignment *)
let ( +:=) x y 
  = x := !x + y

(* physical equality *)
let x = ref 0
let y = x
let z = ref 0
(*
x == y -> true
x == z -> false
x = y -> true
x = z -> true
x := 1 -> ()
x = y -> true
x = z -> false ;;
*)

(* norm *)

(* AF: the float array [| x1; ...; xn |] represents the
 *     vector (x1, ..., xn)
 * RI: the array is non-empty *)
type vector = float array

let norm (vec: vector) = Array.map (Fun.flip ( ** ) 2.) vec 
                         |> (Array.fold_left ( +. ) 0.) 
                         |> Fun.flip ( ** ) 0.5

(* normalize *)

let normalize (vec: vector): unit = 
  Array.map_inplace (Fun.flip (/.) @@ norm vec) vec

(* norm loop *)
let norm_loop (vec: vector): float =
  let n = ref 0. in
  for x=0 to Array.length vec do
    n := !n +. vec.(x) ** 2. ;
  done ;
  !n ** 0.5

(* normalize loop *)
let normalize_loop (vec: vector): unit =
  let n = norm_loop vec in
  for x=0 to Array.length vec do
    vec.(x) <- vec.(x) /. n
  done ; ()

(* init matrix *)
let init_matrix (n: int) (o: int) (f: int -> int -> 'a): 'a array array =
  Array.init n (Fun.compose (Array.init o) f)
