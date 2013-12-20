(* 
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, subject to the conditions
 * listed in the LAC LICENSE file. The Software is provided WITHOUT ANY 
 * WARRANTY, EXPRESS OR IMPLIED. This notice is a summary of the Masstree 
 * LICENSE file; the license in that file is legally binding.
 * 
 * type.ml
 *
 * Define the types for all of the modules and classes.
 *)
open Order

(* set the stype of the state using this module signature.
   make sure this matches the DICT_ARG key *)
module type STATE = 
sig
  type t
  val compare: t -> t -> Order.order
  val string_of_t: t -> string
end

(* Argument module signature to our DICT functors *)
module type DICT_ARG =
sig
  type key
  type value
  val compare : key -> key -> Order.order
  val string_of_key : key -> string
  val string_of_value : value -> string
end

(* module signature for dictionary module *)
module type DICT_T = 
sig
  type key   
  type value 
  type dict

  (* An empty dictionary *)
  val empty : dict 

  val fold : (key -> value -> 'a -> 'a) -> 'a -> dict -> 'a

  (* Returns as an option the value associated with the provided key. If
   * the key is not in the dictionary, return None. *)
  val lookup : dict -> key -> value option

  (* returns the next largest key in the dictionary *)
  val next: dict -> key -> key option

  (* Returns true if and only if the key is in the dictionary. *)
  val member : dict -> key -> bool

  (* Inserts a (key,value) pair into our dictionary. If the key is already
   * in our dictionary, update the key to have the new value. *)
  val insert : dict -> key -> value -> dict

  (* Removes the given key from the dictionary. If the key is not present,
   * return the original dictionary. *)
  val remove : dict -> key -> dict

  (* Return an arbitrary key, value pair along with a new dict with that
   * pair removed. Return None if the input dict is empty *)
  val choose : dict -> (key * value * dict) option

  (* functions to convert our types to strings for debugging and logging *)
  val string_of_key: key -> string
  val string_of_value : value -> string
  val string_of_dict : dict -> string
end

(* Dictionary module/object type that is generated within DFA *)
module type DICT =
sig
  type key
  type value
  class dict :
  object
    method lookup: key -> value option
    method mem: key -> bool
    method add: key -> value -> unit
    method remove: key -> unit
    method fold: 'a. (key->value->'a ->'a) -> 'a -> 'a
  end
end

(* takes in a DICT_ARG and constructs a set based on it. from moogle *)
module type SET_T = 
sig
  type elt  (* type of elements in the set *)
  type set  (* abstract type for the set *)

  val empty : set

  val is_empty : set -> bool

  val insert : elt -> set -> set

  (* same as insert x empty *)
  val singleton : elt -> set

  val union : set -> set -> set
  val intersect : set -> set -> set

  (* remove an element from the set -- if the
   * element isn't present, does nothing. *)
  val remove : elt -> set -> set

  (* returns true iff the element is in the set *)
  val member : set -> elt -> bool

  (* returns the next largest elt of the set *)
  val next: set -> elt -> elt option

  (* chooses some member from the set, removes it 
   * and returns that element plus the new set.  
   * If the set is empty, returns None. *)
  val choose : set -> (elt * set) option

  (* fold a function across the elements of the set
   * in some unspecified order. *)
  val fold : (elt -> 'a -> 'a) -> 'a -> set -> 'a

  (* functions to convert our types to a string. useful for debugging. *)
  val string_of_set : set -> string
  val string_of_elt : elt -> string
  val compare: elt -> elt -> Order.order
end

(* adapted from http://caml.inria.fr/pub/docs/manual-ocaml-4.00/manual007.html#toc38 and moogle *)
(* type for set constructor functor *)
module type SET =
sig
  type elt
  type set
  val empty: set
  class setob: set ->
    object
      method is_empty : bool
      method add : elt -> unit
      method union : setob -> setob
      method intersect : setob -> setob
      method remove : elt -> unit
      method mem : elt -> bool
      method get_set : set
      method fold : 'a. (elt-> 'a -> 'a) -> 'a -> 'a
      method next: elt -> elt option
      method choose: (elt * setob) option
    end
  val compare: setob -> setob -> Order.order
end

(* DFA module/object type *)
module type DFA_T =
sig 
  type state
  type edge = char
  class dfa: state ->
  object
    (* sets a final state for the automaton by updating the set of final 
       states *)
    method set_final_state: state -> unit

    (* takes in a state and returns true if it is in the set of acceptable 
       final states. *)
    method is_final: state -> bool

    (* updates the dictionary of default transitions to set a specific one *)
    method set_default_transition: state -> state -> unit

    (* updates the dictionary of transitions within the given automaton object
       to include the new one *)
    method set_transition: state -> edge -> state -> unit

    (* returns the new state resulting from the passed state and edge.  
       Returns None if there isn’t one *)
    method next_state: state -> edge -> state option

    (* will return the next smallest edge for a given state *)
    method next_edge: state -> edge option -> edge option

    (* iterates through the DFA by taking each character of the string as an edge.
       Goes as far as possible to start out with by using a stack implementation 
       and popping off the most recent entry then calling next_edge and is_final 
       to determine what the next final state is.  Concurrently, it builds a string
       from the edges used, and returns this once a final state is reached. *)
    method next_valid_word: string -> string option
  end
end

(* module type for the NFA *)
module type NFA_T =
sig 
  type state
  type set
  type dict
  type dfa_type
  type edge = char
  class nfa: state list ->
  object
    (* updates the dictionary of transitions within the given nfa object to 
       include the new one *)
    method set_transition: state -> edge -> state -> unit
    
    (* updates the set of acceptable final states to include the passed in 
       one *)
    method set_final_state: state -> unit
    
    (* filters a set to return only members of the set that are final states*)
    method is_final: set -> set
  
    (* takes in a set of states and an edge and returns the next states using 
       that edge *)
    method next: set -> edge -> set

    (* construct a dfa from the nfa *)
    method construct : unit

    (* gives the next valid word for the constructed DFA *)
    method next_valid_word: string -> string option
  end
end