(* 
 * states.ml
 * 
 * defines a bunch of different state representations 
 * for use in creating dfas and nfas.
 *)

open Type
open Order
open Sets

(* int states *)
module IntType : STATE with type t = int =
struct
  type t = int
  let compare x y = if x < y then Less else if x > y then Greater else Eq
  let string_of_t = string_of_int
end

(* string states *)
module StringType: STATE with type t = string =
struct
  type t = string
  let compare x y = if x < y then Less else if x > y then Greater else Eq
  let string_of_t t = t
end

(* int tuple states used for NFAs *)
module IntTupleType : STATE with type t = int*int =
struct
  type t = int*int
  let compare x y =
    let (a,b) = x in 
    let (c,d) = y in
    if a < c then Less else if a > c then Greater 
    else
        if b < d then Less else if b > d then Greater else Eq
  let string_of_t x = 
    let (a,b) = x in 
    "("^(string_of_int a)^","^(string_of_int b)^")"
end

(* Set state types used for the powerset DFA *)
module SetType =
  functor (ST: STATE) ->
struct 
  module S: SET with type elt = ST.t = MakeSet(ST)
  type t = S.setob
  let compare (x:t) (y:t) =
    let rec r_comp (x:t) (y:t) = 
      match x#choose, y#choose with
      | None, None -> Eq
      | Some (_,_), None
      | None, Some (_,_) -> Greater
      | Some (e1, s1), Some (e2,s2) -> 
        let comp = ST.compare e1 e2 in 
        if comp = Eq then r_comp s1 s2
        else comp 
    in
    r_comp x y
  let string_of_t (x:t) = 
    x#fold (fun x y -> (ST.string_of_t x)^"\n"^y) ""
end