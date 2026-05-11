(* Poly spec *)

(** [Poly] represents immutable poolynomials with integer coefficients. *)
module type Poly = sig
  (** [t] is the type of polynomials. *)
  type t

  (** [val x p] is [p] evaluated at [x].
      Example: if [p] represents $3x^3+x^2+x, then [eval 10 p] is [3110].  *)
  val eval : int -> t -> int

  (** [create l] creates a polynomial from a list of coefficients, where
      the 0-index term of the list is the coefficient of $x^0$, the 1-index is
      the coefficent of $x^1$, and so on such that the n-index is the coefficient of $x^2$.
      Example: [create [ 0 ; 1 ; 1 ; 3 ]] should create the polynomial
               representing $3x^3+x^2+x$ *)
  val create : int list -> t

  (** [add p q] is [p] added to [q].
      Example: if [p] represents $x^2$ and [q] represents $x^2 + 1$ then
               [add p q] is a new polynomial representing $2x^2+1$. *)
  val add : t -> t -> t

  (** [neg p] is the negative polynomial of [p].
      Example: if [p] represents $3x^3+x^2+x$, then [neg p] is a new 
               polynomial representing $-3x^3-x^2-x$ *)
  val neg : t -> t

  (** [coefficients p] is a list of coefficents of p, in reverse order of
      power, so that [create @@ coeffiecients p] is always equal to p.
      Example: if [p] represents $3x^2 + x^2 + 2$ then [coefficients p] is
               [[2 ; 0 ; 1 ; 2]] *)
  val coefficients : t -> int list

  (* other things that could go here:
     1. degree
     2. products
     3. make this a Functor working on an underlying ring *)
end

(* poly impl *)

module PolyList : Poly = struct
  (* Represents a polynomial via a list of the coefficients, in reverse order
     of appearance. 
     Example: if the polynomial is $3x^2 + x^2 + 2$ then the representation is
              [2 ; 0 ; 1 ; 3].
   *)
  type t = int list

  let rec eval x p = match p with
    | [] -> 0
    | c :: p' -> c + x * eval x p'

  let rec add p q = match p, q with
    | (_, []) -> p
    | ([], _) -> q
    | (c1 :: p', c2 :: q') -> (c1 + c2) :: (add p' q')

  let neg = List.map (~-)

  let create = Fun.id 

  let coefficients = Fun.id
end

(* interval arithmatic *)

exception Invalid_interval
exception No_overlap
exception Zero_division
(* [Interval] represents intervals, with each end being closed *)
module type Interval = sig
  (** [t] is the type of the interval *)
  type t

  (** [rep_ok i] is i if the invariant is upheld. Otherwise,
      it throws [Invalid_interval *)
  val rep_ok : t -> t

  (** [make a b] creates an interval from a to b. If a > b, it makes
      an empty interval *)
  val make : float -> float -> t

  (** [i = j] is true if and only if i and j represent the
      same (potentially empty) interval *)
  val ( = ) : t -> t -> bool

  (** [empty i] is true if i represents an empty interval *)
  val empty : t -> bool

  (** [intersect i j] returns the (potentially empty) intersection
      of intervals i and j *)
  val intersect : t -> t -> t

  (** [overlap i j] returns true if and only if i and j overlap *)
  val overlap : t -> t -> bool

  (** [union i j] returns the union of two intervals, if they overlap.
      If i or j is empty, returns the other. If both are empty, returns an
      empty inverval. If they do not overlap, Raises [No overlap] *)
  val union : t -> t -> t 

  (** [i < j] is true if and only if (i represents [a, b], j represents [c, d],
      and b < d) and neither is empty. This could mean that i < j and j < i are both false. 
      Empty is treated as being before everything, except for itself. *)
  val ( < ) : t -> t -> bool

  (** [width i] is how wide the interval is. If i represents [a, b], then
      the width is b - a; unless i is empty, in which case the width is 0. *)
  val width : t -> float

  (** [abs i] = max(|a|, |b|) if i is nonempty, 0 if it is empty *)
  val abs : t -> float

  (** [i + j] = k (with i = [a, b], j = [c, d]) where k = [a + c, b + d] *)
  val ( + ) : t -> t -> t

  (** [i - j] = k (with i = [a, b], j = [c, d]) where k = [a - c, b - d] *)
  val ( - ) : t -> t -> t

  (** [i * j] = k (with i = [a, b], j = [c, d]) where 
      k = [min(ac, ad, bc, bd), max(ac, ad, bc, bd)] *)
  val ( * ) : t -> t -> t

  (** [i / j] = k (with i = [a, b], j = [c, d]) where 
      k = [min(a/c, a/d, b/c, b/d), max(a/c, a/d, b/c, b/d)]. If
      0 is in j, then Raises [Zero_division] *)
  val ( / ) : t -> t -> t

  (** [to_string i] returns "empty" if t is an empty interval;
      if it's nonempty with i = [a, b] then it returns "[a, b]" *)
  val to_string : t -> string

  (** [printer f i] prints i with respect to the formatting f *)
  val printer : Format.formatter -> t -> unit
end

module FloatInterval : Interval = struct
  (* Abstraction function: Inverval (a, b) represents the closed interval
   * [a, b] with a <= b and Empty represents an empty interval. 
   *)
  type t = | Empty | Interval of float * float

  let unary_handle empty_const interval_func i =
    match i with
    | Empty -> empty_const
    | Interval (a, b) -> interval_func (a, b)

  let binary_handle 
    (empty_const : 'a) 
    (first_interval_func : float * float -> 'a)
    (second_interval_func : float * float -> 'a)
    (both_intervals_func : float * float -> float * float -> 'a)
    (i : t) (j : t) : 'a = match i, j with
    | Empty, Empty -> empty_const
    | Interval (a, b), Empty -> first_interval_func (a, b)
    | Empty, Interval (c, d) -> second_interval_func (c, d)
    | Interval (a, b), Interval (c, d) -> both_intervals_func (a, b) (c, d)

  let rep_ok = unary_handle Empty @@ fun (a, b) -> 
    if a <= b then Interval (a, b) else raise Invalid_interval

  let make a b =
    if a <= b then Interval (a, b) else Empty

  let empty = unary_handle false @@ fun _ -> true

  let ( = ) = binary_handle true (Fun.const false) (Fun.const false) @@
    fun (a, b) (c, d) -> a = c && b = d

  let intersect = binary_handle Empty (Fun.const Empty) (Fun.const Empty) @@
    fun (a, b) (c, d) -> 
      let start = max a c in
      let end' = min b d in
    if start <= end' then Interval (start, end') else Empty

  let overlap i j = not @@ empty @@ intersect i j

  let union =
    binary_handle
      (Empty)
      (Fun.compose raise @@ Fun.const No_overlap)
      (Fun.compose raise @@ Fun.const No_overlap) @@
      fun (a, b) (c, d) -> 
        if b < c && d < d
        then raise No_overlap
        else Interval (min a c, max b d)

  let ( < ) = binary_handle false (Fun.const true) (Fun.const false) @@
    fun (_, b) (c, _) -> b < c

  let width = unary_handle 0. @@ fun (a, b) -> b -. a

  let abs = unary_handle 0. @@ fun (a, b) -> max (abs_float a) (abs_float b)

  let min4 w x y z = min (min w x) (min y z)
  let max4 w x y z = max (max w x) (max y z)

  let ( + ) = binary_handle Empty (Fun.const Empty) (Fun.const Empty) @@
    fun (a, b) (c, d) -> Interval (a +. c, b +. d)

  let ( - ) = binary_handle Empty (Fun.const Empty) (Fun.const Empty) @@
    fun (a, b) (c, d) -> Interval (a -. c, b -. d)

  let ( * ) = binary_handle Empty (Fun.const Empty) (Fun.const Empty) @@
    fun (a, b) (c, d) -> 
      let start = min4 (a *. c) (a *. d) (b *. c) (b *. d) in
      let end'  = max4 (a *. c) (a *. d) (b *. c) (b *. d) in
    (* by definition, start <= end' so no empty check needed *)
    Interval (start, end')

  let ( / ) = binary_handle Empty (Fun.const Empty) (Fun.const Empty) @@
    fun (a, b) (c, d) -> 
      if c <= 0. && 0. <= d then raise Zero_division else () ;
      let start = min4 (a /. c) (a /. d) (b /. c) (b /. d) in
      let end'  = max4 (a /. c) (a /. d) (b /. c) (b /. d) in
      (* by definition, start <= end' so no empty check needed *)
      Interval (start, end')

  let to_string = unary_handle "Empty" @@
    fun (a, b) -> "[" ^ string_of_float a ^ ", " ^ string_of_float b ^ "]"

  let printer fmt = Fun.compose (Format.fprintf fmt "%s") to_string
end

(* function maps *)
module type Dictionary = sig
  (** ('k, 'v) t is the type of the Dictionary*)
  type ('k, 'v) t

  (** [empty] is an empty Dictionary *)
  val empty : ('k, 'v) t

  (** [mem key dict] is true when key is an in the dictionary *)
  val mem : 'k -> ('k, 'v) t -> bool

  (** [find key dict] is the value of the key in the dictionary.
      If the key is not used, raised [Not_found] *)
  val find : 'k -> ('k, 'v) t -> 'v

  (** [add key val dict] is a new dictionary with key/val added,
      overwriting any existing definition of `key` *)
  val add : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

  (** [remove key val dict] is a new dictionary with key removed,
      if it exists. *)
  val remove : 'k -> ('k, 'v) t -> ('k, 'v) t 
end

module FunctionDictionary : Dictionary = struct
  (** The dictionary is saved as a function from keys to values,
      with unknown keys causing the function to throw [Not_found] *)
  type ('k, 'v) t = 'k -> 'v

  let empty : ('k, 'v) t = fun _ -> raise Not_found

  let mem (key : 'k) (dict : 'k -> 'v) : bool = try
    let _ = dict key
    in true
  with Not_found -> false

  let find key dict = dict key

  let add key v dict = (fun k ->
    if key = k then v else dict k)

  let remove key dict = fun k ->
    if key <> k then dict k else raise Not_found 
end

(* skipping the testing and the 'proof' exercises *)

(* propositions *)

type proposition = 
  | Atomic
  | Negation of proposition
  | Conjuction  of proposition * proposition
  | Disjuction  of proposition * proposition 
  | Implication of proposition * proposition

(*
forall Properties P,
  if P(Atomic)
  and if 
    P(p) implies P(Negation p)
    P(p1) and P(p2) implies P(Conjunction p1 p2)
    P(p1) or P(p2) implies P(Disjunction p1 p2)
    P(p1) => P(p2) implise P(Implicatino p1 p2)
  then forall p, P(p)
*)

(* list spec *)
(* skipped *)

(* bag spec *)
module type Bag = sig
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val insert : 'a -> 'a t -> 'a t
  val mult : 'a -> 'a t -> int
  val remove : 'a -> 'a t -> 'a t
end

(*
generators:
  empty
manipulators:
  insert
  remove
queries:
  mult

is_empty empty = true
is_empty (insert x b) = false
mult x (insert x b) = 1 + (mult b x)
mult x (remove x b) = min (mult b x - 1) 0
*)
