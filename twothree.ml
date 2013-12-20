(*  
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, subject to the conditions
 * listed in the LAC LICENSE file. The Software is provided WITHOUT ANY 
 * WARRANTY, EXPRESS OR IMPLIED. This notice is a summary of the Masstree 
 * LICENSE file; the license in that file is legally binding.
 * 
 * twothree.ml
 *
 * This is the implementation of a twothree tree from moogle.
 * Also defines the dictionary module that we will be using
 * which contains the constuctor for dictionary classes.
 *)
open Type

(* twothree tree dictionary from moogle *)
module BTDict(D:DICT_ARG) : (DICT_T with type key = D.key
with type value = D.value) =
struct
  open Order

  exception TODO
  exception IncorrectTree

  type key = D.key
  type value = D.value

  type pair = key * value

  type dict = 
    | Leaf
    | Two of dict * pair * dict
    | Three of dict * pair * dict * pair * dict

  type kicked =
    | Up of dict * pair * dict
    | Done of dict

  type hole =
    | Hole of pair option * dict
    | Absorbed of pair option * dict

  type direction2 =
    | Left2
    | Right2

  type direction3 =
    | Left3
    | Mid3
    | Right3
        
  (* How do we represent an empty dictionary with 2-3 trees? *)
  let empty : dict = Leaf

  (* TODO:
   * Implement fold. Read the specification in the DICT signature above. *)

  let rec fold (f: key -> value -> 'a -> 'a) (u: 'a) (d: dict) : 'a =
    match d with
    | Leaf -> u 
    | Two (d1,(k,v),d2) -> f k v (fold f (fold f u d2) d1)
    | Three (d1, (k1,v1), d2, (k2,v2), d3) -> 
      f k1 v1 (f k2 v2 (fold f (fold f (fold f u d1) d2) d3))

  (* TODO:
   * Implement these to-string functions *)
  let string_of_key = D.string_of_key
  let string_of_value = D.string_of_value

  let rec string_of_tree (d: dict) : string = 
    match d with
      | Leaf -> "Leaf"
      | Two(left,(k,v),right) -> "Two(" ^ (string_of_tree left) 
        ^ ",(" ^ (string_of_key k) ^ "," ^ (string_of_value v) ^ "),"
        ^ (string_of_tree right) ^ ")"
      | Three(left,(k1,v1),middle,(k2,v2),right) -> 
        "Three(" ^ (string_of_tree left)
        ^ ",(" ^ (string_of_key k1) ^ "," ^ (string_of_value v1) ^ "),"
        ^ (string_of_tree middle) ^ ",(" ^ (string_of_key k2) ^ "," 
        ^ (string_of_value v2) ^ ")," ^ (string_of_tree right) ^ ")"

  let string_of_dict (d: dict) : string = string_of_tree d 

  let insert_upward_two (w: pair) (w_left: dict) (w_right: dict) 
      (x: pair) (x_other: dict) : kicked = 
    let (wa, _) = w in 
    let (xa, _) = x in 
    match D.compare wa xa with 
    | Eq -> failwith "twothree 00" (* raise IncorrectTree *)
    | Less -> Done (Three (w_left, w, w_right, x, x_other))
    | Greater -> Done (Three (x_other, x, w_left, w, w_right))

  let insert_upward_three (w: pair) (w_left: dict) (w_right: dict)
      (x: pair) (y: pair) (other_left: dict) (other_right: dict) : kicked =
    let (xa,_) = x in 
    let (ya, _) = y in 
    let (wa, _) = w in 
    match D.compare wa xa with
    | Eq -> failwith "twothree 01" (* raise IncorrectTree *)
    | Less -> 
      Up (Two (w_left,w,w_right),x, Two (other_left,y,other_right))
    | Greater ->
      match D.compare wa ya with
      | Eq -> failwith "twothree 02" (* raise IncorrectTree *)
      | Less -> 
        Up (Two (other_left,x,w_left),w, Two (w_right, y, other_right))
      | Greater -> 
        Up (Two (other_left, x, other_right),y, Two (w_left,w,w_right))

  let rec insert_downward (d: dict) (k: key) (v: value) : kicked =
    match d with
      | Leaf -> Up (Leaf, (k,v), Leaf) (* base case! see handout *)
      | Two(left,n,right) -> insert_downward_two (k,v) n left right
      | Three(left,n1,middle,n2,right) -> 
          insert_downward_three (k,v) n1 n2 left middle right

  and insert_downward_two ((k,v): pair) ((k1,v1): pair) 
      (left: dict) (right: dict) : kicked = 
    match D.compare k k1 with
    | Eq -> Done (Two (left, (k,v), right))
    | Less -> 
      (match insert_downward left k v with
        | Up(l,(k2,v2),r) -> insert_upward_two (k2,v2) l r (k1,v1) right
        | Done x -> Done (Two (x, (k1,v1), right)))
    | Greater -> 
      (match insert_downward right k v with
        | Up(l,(k2,v2),r) -> insert_upward_two (k2,v2) l r (k1,v1) left
        | Done x -> Done (Two(left, (k1,v1), x)))

  and insert_downward_three ((k,v): pair) ((k1,v1): pair) ((k2,v2): pair) 
      (left: dict) (middle: dict) (right: dict) : kicked =
    match D.compare k k1 with
    | Eq -> Done (Three (left, (k,v), middle, (k2,v2), right))
    | Less -> 
      (match insert_downward left k v with
        | Up(l,(k3,v3),r) -> insert_upward_three (k3,v3) l r (k1,v1) (k2,v2) middle right
        | Done x -> Done (Three (x,(k1,v1),middle,(k2,v2),right)))
    | Greater ->
      (match D.compare k k2 with
        | Eq -> Done (Three (left, (k1,v1), middle, (k,v), right))
        | Less -> 
          (match insert_downward middle k v with 
            | Up(l,(k3,v3),r) -> insert_upward_three (k3,v3) l r (k1,v1) (k2,v2) left right
            | Done x -> Done (Three (left, (k1,v1), x, (k2,v2), right)))
        | Greater -> 
          (match insert_downward right k v with
            | Up(l,(k3,v3),r) -> insert_upward_three (k3,v3) l r (k1,v1) (k2,v2) left middle
            | Done x -> Done (Three (left, (k1,v1), middle, (k2,v2), x))))

  let insert (d: dict) (k: key) (v: value) : dict =
    match insert_downward d k v with
      | Up(l,(k1,v1),r) -> Two(l,(k1,v1),r)
      | Done x -> x  

  let remove_upward_two (n: pair) (rem: pair option) 
      (left: dict) (right: dict) (dir: direction2) : hole =
    match dir,n,left,right with
      | Left2,x,l,Two(m,y,r) -> Hole(rem,Three(l,x,m,y,r))
      | Right2,y,Two(l,x,m),r -> Hole(rem,Three(l,x,m,y,r))
      | Left2,x,a,Three(b,y,c,z,d) -> 
          Absorbed (rem, Two(Two(a,x,b),y,Two(c,z,d)))
      | Right2,z,Three(a,x,b,y,c),d -> 
          Absorbed (rem, Two(Two(a,x,b),y,Two(c,z,d)))
      | Left2,_,_,_ | Right2,_,_,_ -> Absorbed(rem,Two(Leaf,n,Leaf))

  let remove_upward_three (n1: pair) (n2: pair) (rem: pair option)
      (left: dict) (middle: dict) (right: dict) (dir: direction3) : hole =
    match dir,n1,n2,left,middle,right with
      | Left3,x,z,a,Two(b,y,c),d -> Absorbed(rem,Two(Three(a,x,b,y,c),z,d))
      | Mid3,y,z,Two(a,x,b),c,d -> Absorbed(rem,Two(Three(a,x,b,y,c),z,d))
      | Mid3,x,y,a,b,Two(c,z,d) -> Absorbed(rem, Two(a,x,Three (b,y,c,z,d)))
      | Right3,x,z,a,Two(b,y,c),d -> Absorbed(rem, Two(a,x,Three (b,y,c,z,d)))
      | Left3,w,z,a,Three(b,x,c,y,d),e -> 
          Absorbed(rem, Three(Two(a,w,b),x,Two(c,y,d),z,e)) 
      | Mid3,y,z,Three(a,w,b,x,c),d,e -> 
          Absorbed(rem, Three(Two(a,w,b),x,Two(c,y,d),z,e))
      | Mid3,w,x,a,b,Three(c,y,d,z,e) -> 
          Absorbed(rem, Three(a,w,Two(b,x,c),y,Two(d,z,e)))
      | Right3,w,z,a,Three(b,x,c,y,d),e -> 
          Absorbed(rem, Three(a,w,Two(b,x,c),y,Two(d,z,e)))
      | Left3,_,_,_,_,_ | Mid3,_,_,_,_,_ | Right3,_,_,_,_,_ ->
        Absorbed(rem,Three(Leaf,n1,Leaf,n2,Leaf))

  (* DO NOT EDIT THIS *)
  let rec remove_downward (d: dict) (k: key) : hole =
    match d with
      | Leaf -> Absorbed(None,d)
      | Two(Leaf,(k1,v1),Leaf) ->
        (match D.compare k k1 with
          | Eq -> Hole(Some(k1,v1),Leaf)
          | Less | Greater -> Absorbed(None,d)
        )
      | Three(Leaf,(k1,v1),Leaf,(k2,v2),Leaf) ->
        (match D.compare k k1, D.compare k k2 with
          | Eq, _ -> Absorbed(Some(k1,v1),Two(Leaf,(k2,v2),Leaf))
          | _, Eq -> Absorbed(Some(k2,v2),Two(Leaf,(k1,v1),Leaf))
          | _, _ -> Absorbed(None,d)
        )
      | Two(l,n,r) -> remove_downward_two k n l r
      | Three(l,n1,m,n2,r) -> remove_downward_three k n1 n2 l m r

  (* DO NOT EDIT THIS *)
  and remove_downward_two (k: key) ((k1,v1): pair) 
      (left: dict) (right: dict) : hole =
    match D.compare k k1 with
      | Eq ->
        (match remove_min right with
          | Hole(None,_) -> Hole(None,left)
          | Hole(Some n,new_right) -> 
            remove_upward_two n None left new_right Right2
          | Absorbed(None,_) -> Hole(None,left)
          | Absorbed(Some n,new_right) -> Absorbed(None,Two(left,n,new_right))
        )
      | Less -> 
        (match remove_downward left k with
          | Hole(rem,t) -> remove_upward_two (k1,v1) rem t right Left2
          | Absorbed(rem,t) -> Absorbed(rem,Two(t,(k1,v1),right))
        )
      | Greater ->
        (match remove_downward right k with
          | Hole(rem,t) -> remove_upward_two (k1,v1) rem left t Right2
          | Absorbed(rem,t) -> Absorbed(rem,Two(left,(k1,v1),t))
        )

  (* DO NOT EDIT THIS *)
  and remove_downward_three (k: key) ((k1,v1): pair) ((k2,v2): pair)
      (left: dict) (middle: dict) (right: dict) : hole =
    match D.compare k k1, D.compare k k2 with
      | Eq, _ ->
        (match remove_min middle with
          | Hole(None,_) -> Hole(None,Two(left,(k2,v2),right))
          | Hole(Some n,new_middle) -> 
            remove_upward_three n (k2,v2) None left new_middle right Mid3
          | Absorbed(None,_) -> Absorbed(None,Two(left,(k1,v1),right))
          | Absorbed(Some n,new_middle) -> 
            Absorbed(None,Three(left,n,new_middle,(k2,v2),right))
        )
      | _ , Eq ->
        (match remove_min right with
          | Hole(None,_) -> Hole(None,Two(left,(k1,v1),middle))
          | Hole(Some n,new_right) -> 
            remove_upward_three (k1,v1) n None left middle new_right Right3
          | Absorbed(None,_) -> Absorbed(None,Two(left,(k1,v1),middle))
          | Absorbed(Some n,new_right) -> 
            Absorbed(None,Three(left,(k1,v1),middle,n,new_right))
        )
      | Less, _ ->
        (match remove_downward left k with
          | Hole(rem,t) -> 
            remove_upward_three (k1,v1) (k2,v2) rem t middle right Left3
          | Absorbed(rem,t) -> 
            Absorbed(rem,Three(t,(k1,v1),middle,(k2,v2),right))
        )
      | _, Greater ->
        (match remove_downward right k with
          | Hole(rem,t) -> 
            remove_upward_three (k1,v1) (k2,v2) rem left middle t Right3
          | Absorbed(rem,t) -> 
            Absorbed(rem,Three(left,(k1,v1),middle,(k2,v2),t))
        )
      | Greater, Less ->
        (match remove_downward middle k with
          | Hole(rem,t) -> 
            remove_upward_three (k1,v1) (k2,v2) rem left t right Mid3
          | Absorbed(rem,t) -> 
            Absorbed(rem,Three(left,(k1,v1),t,(k2,v2),right))
        )

  (* DO NOT EDIT THIS *)
  and remove_min (d: dict) : hole =
    match d with
      | Leaf -> Hole(None,Leaf)
      | Two(Leaf,n,_) -> Hole(Some n,Leaf)
      | Three(Leaf,n1,middle,n2,right) -> Absorbed(Some n1,Two(middle,n2,right))
      | Two(left,n,right) -> 
        (match remove_min left with
          | Hole(rem,t) -> remove_upward_two n rem t right Left2
          | Absorbed(rem,t) -> Absorbed(rem,Two(t,n,right))
        )
      | Three(left,n1,middle,n2,right) ->
        (match remove_min left with
          | Hole(rem,t) -> remove_upward_three n1 n2 rem t middle right Left3
          | Absorbed(rem,t) -> Absorbed(rem,Three(t,n1,middle,n2,right))
        )

  let remove (d: dict) (k: key) : dict =
    match remove_downward d k with
      | Hole(_,d') -> d'
      | Absorbed(_,d') -> d'

  let rec lookup (d: dict) (k: key) : value option =
    match d with 
    | Leaf -> None
    | Two (d1, (k1,v), d2) -> 
      (match D.compare k k1 with 
        | Eq -> Some v
        | Less -> lookup d1 k 
        | Greater -> lookup d2 k)
    | Three (d1, (k1,v1), d2, (k2,v2), d3) ->
      (match D.compare k k1, D.compare k k2 with
        | Eq, Less -> Some v1
        | Eq, _ -> failwith "twothree 03" (* raise IncorrectTree *)
        | Greater, Eq -> Some v2
        | _, Eq -> failwith "twothree 04" (* raise IncorrectTree *)
        | Less, Less -> lookup d1 k 
        | Less, Greater -> failwith "twothree 05" (* raise IncorrectTree *)
        | Greater, Less -> lookup d2 k 
        | Greater, Greater -> lookup d3 k )

  let member (d: dict) (k: key) : bool =
    match lookup d k with
    | None -> false
    | Some _ -> true

  let choose (d: dict) : (key * value * dict) option =
    match d with 
    | Leaf -> None
    | Two (d1, (k,v), d2) -> Some (k, v, (remove d k))
    | Three (d1, (k1,v1), d2, (k2,v2), d3) -> Some (k1, v1, (remove d k1))

  let rec least (d:dict): key option =
    match d with
    | Leaf -> None
    | Two (d1, (k,v), d2) -> if d1 = Leaf then Some k else least d1
    | Three (d1, (k1,v1), d2, (k2,v2), d3) -> if d1 = Leaf then 
                                                  Some k1 else least d1

  (* returns the next smallest element *)
  let rec next (d: dict) (k: key) : key option =
    match d with 
    | Leaf -> None
    | Two (d1, (k1,v), d2) -> 
      (match D.compare k k1 with 
        | Eq -> 
          (match least d2 with
          | None -> None
          | Some kn -> Some kn)
        | Less -> 
          (match next d1 k with 
          | None -> Some k1
          | Some kn -> Some kn)
        | Greater -> next d2 k)
    | Three (d1, (k1,v1), d2, (k2,v2), d3) ->
      (match D.compare k k1, D.compare k k2 with
        | Eq, Less -> 
          (match least d2 with
          | None -> Some k2
          | Some kn -> Some kn)
        | Eq, _ -> failwith "twothree 06" (* raise IncorrectTree *)
        | Greater, Eq -> 
          (match least d3 with
            | None -> None
            | Some kn -> Some kn)
        | _, Eq -> failwith "twothree 07" (* raise IncorrectTree *)
        | Less, Less -> 
          (match next d1 k with
          | None -> Some k1
          | Some kn -> Some kn)
        | Less, Greater -> failwith "twothree 08" (* raise IncorrectTree *)
        | Greater, Less -> 
          (match next d2 k with 
          | None -> Some k2
          | Some kn -> Some kn) 
        | Greater, Greater -> next d3 k )

  let balanced (d: dict) : bool =
    let rec bal_help (d:dict) (i:int) : int =
      match d with 
      | Leaf -> i
      | Two (l,_,r) -> 
        let lh = bal_help l (i + 1) in 
        let rh = bal_help r (i + 1) in 
        if lh = rh then 
          lh 
        else -1
      | Three (l,_,m,_,r) ->
        let lh = bal_help l (i + 1) in 
        let mh = bal_help m (i + 1) in 
        let rh = bal_help r (i + 1) in 
        if lh = rh && mh = rh then 
          lh 
        else -1
    in
    not(bal_help d 0 = -1)
end

(* functor to make dictionary.  Used in dictset *)
module Make (D:DICT_ARG) : (DICT_T with type key = D.key
  with type value = D.value) = 
    BTDict(D)

(* functor constructor for dictionary *)
module MakeDict (D:DICT_ARG): DICT with type key = D.key 
with type value = D.value = 
struct
  type key = D.key
  type value = D.value
  module D= BTDict(D)
  class dict =
  object
    val mutable d = D.empty
    method lookup k = D.lookup d k
    method mem k = D.member d k 
    method add k v = d <- D.insert d k v
    method remove k = d <- D.remove d k
    method fold:'a. (key->value->'a ->'a) -> 'a -> 'a = fun f u -> D.fold f u d
  end
end
