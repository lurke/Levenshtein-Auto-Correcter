(* 
 * sets.ml
 *
 * Defines sets in terms of hte twothree tree defined in twothree.ml
 * used in moogle.
 * Also defines the sets module which contains the set class
 * constructor.
 *)

open Type
open Order
open Twothree

(* twothree tree dictionary set from moogle *)
module DictSet(C : STATE) : (SET_T with type elt = C.t) = 
struct
  module D = Twothree.Make(struct
      type key = C.t
      type value = C.t
      let compare = C.compare
      let string_of_key = C.string_of_t
      let string_of_value = C.string_of_t
  end)

  type elt = D.key
  type set = D.dict
  let empty = D.empty

  let is_empty (s:set) : bool = 
    s = empty

  let insert (e: elt) (s:set) =
    D.insert s e e

  (* same as insert x empty *)
  let singleton (e:elt) : set = 
    D.insert empty e e

  let rec union (x:set) (y:set) : set = 
    match D.choose y with
    | None -> x
    | Some (k,v,s) -> union (insert k x) s

  let intersect (x:set) (y:set) : set= 
    let rec inthelp (x:set) (y:set) (z:set) = 
      match D.choose y with
      | None -> z
      | Some (k,v,s) -> 
        if D.member x k then inthelp x s (insert k z)
        else
          inthelp x s z
    in
    inthelp x y empty

  (* remove an element from the set -- if the
   * element isn't present, does nothing. *)
  let remove (e:elt) (s:set) : set =
    D.remove s e

  (* returns true iff the element is in the set *)
  let member (s:set) (e:elt) : bool =
    D.member s e

  (* gives the next largest elt in the set*)
  let next (s:set) (e:elt): elt option =
    D.next s e

  (* chooses some member from the set, removes it 
   * and returns that element plus the new set.  
   * If the set is empty, returns None. *)
  let choose (s:set) : (elt * set) option =
    match D.choose s with
    | None -> None
    | Some (k,v,s) -> Some (k,s)

  (* fold a function across the elements of the set
   * in some unspecified order. *)
  let fold (f: elt -> 'a -> 'a) (x: 'a) (s:set) : 'a =
    D.fold (fun k v b -> f k b) x s

  let string_of_elt = D.string_of_key 
  let string_of_set s = D.string_of_dict s
  let compare = C.compare
end

(* set functor constuctor *)
module Set (S:SET_T): SET with type elt = S.elt =
struct
  type elt = S.elt
  type set = S.set
  let empty = S.empty
  class setob start =
    object
      val mutable s = start
      method is_empty = S.is_empty s
      method mem x = S.member s x
      method add x = s <- S.insert x s
      method union (t:setob) = new setob (S.union s (t#get_set))
      method intersect (t:setob) = new setob (S.intersect s (t#get_set))
      method remove x = s <- S.remove x s 
      method get_set = s
      method fold: 'a. (elt-> 'a -> 'a) -> 'a -> 'a = fun f x -> S.fold f x s
      method next x = S.next s x
      method choose: (elt * setob) option = 
        match S.choose s with 
        | None -> None 
        | Some (elt, s) -> Some (elt, (new setob s))
    end
  let compare (x: setob) (y:setob) = 
    let rec r_comp (x:setob) (y:setob) = 
      match x#choose, y#choose with
      | None, None -> Eq
      | Some (_,_), None
      | None, Some (_,_) -> Greater
      | Some (e1, s1), Some (e2,s2) -> 
        let comp = S.compare e1 e2 in 
        if comp = Eq then r_comp s1 s2
        else comp
    in
    let xn = x#fold (fun a b -> 1 + b) 0 in 
    let yn = y#fold (fun a b -> 1 + b) 0 in 
    if xn < yn then Less
    else if xn > yn then Greater 
    else 
      r_comp x y
end

(* functor to make the set *)
module MakeSet (S:STATE)  =
  Set (DictSet(S))
