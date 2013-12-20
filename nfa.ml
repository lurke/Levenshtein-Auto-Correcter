(* 
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, subject to the conditions
 * listed in the LAC LICENSE file. The Software is provided WITHOUT ANY 
 * WARRANTY, EXPRESS OR IMPLIED. This notice is a summary of the Masstree 
 * LICENSE file; the license in that file is legally binding.
 * 
 * nfa.ml
 * 
 * Defines the module functor/object constuctor for NFA.
 *)

open Order
open Type
open Twothree
open Sets
open Dfa
open States

module NFA (State: STATE) :
  NFA_T with type state = State.t =
struct
  type state = State.t
  type edge = char 
  module S: SET with type elt = State.t = MakeSet(State)
  type set = S.setob
  (* define a dictionary for transitions *)
  module Trans = 
    MakeDict(struct
      type key = state * char
      type value = set
      let compare (s1, c1) (s2, c2) = 
        match State.compare s1 s2 with
        | Less -> Less
        | Greater -> Greater
        | Eq -> (Order.char_compare c1 c2)
      let string_of_key (s, c) = 
        "(" ^ (State.string_of_t s) ^ ", " ^ 
      (String.make 1 c) ^ ")"
      let string_of_value v = ""
    end)
  type dict = Trans.dict
  module DFA: DFA_T with type state = set = 
    DFA (struct 
          type t = S.setob
          let compare = S.compare
          let string_of_t (x:t) = 
            x#fold (fun x y -> (State.string_of_t x)^"\n"^y) ""
        end)
  type dfa_type = DFA.dfa
  class nfa (starts: state list) =
  object(self)
    (* Instance Variables *)
    val start_state = new S.setob S.empty
    val final_states = new S.setob S.empty
    val transitions = new Trans.dict
    val mutable dfa_final = new DFA.dfa (new S.setob S.empty)

    (* Initializer: adds the start states to the start state set *)
    initializer
      List.iter (fun x -> start_state#add x) starts

    method set_final_state (s:state) = final_states#add s

    method set_transition (s:state) (e:edge) (n:state) = 
      let k = (s, e) in
      match transitions#lookup k with
      | None -> 
          let new_val = new S.setob S.empty in new_val#add n;
          transitions#add k new_val
      | Some v -> v#add n
    
    method is_final (ss:set) = final_states#intersect ss

    method next (ss:set) (e:edge): set =
      let funct st se = 
        match transitions#lookup (st, e) with
        | None -> se
        | Some v -> v#union se
      in
      ss#fold funct (new S.setob S.empty)

    (* checks whether the given set is in the set Queue *)
    method private in_queue (st:set) (q: set Queue.t) : bool  =
        Queue.fold (fun x y -> 
          if (S.compare st y) = Eq then true else (x || false)) 
          false q

    (* returns a set of chars that are from each state *)
    method private explore_edges (dfa_state: set): char list =
      let get_edges st =
        transitions#fold 
          (fun x _ lst -> let (a,b) = x in 
            if (State.compare st a = Eq )then b::lst else lst
          ) []
      in 
      dfa_state#fold (fun x lst -> (get_edges x)@lst) []

    (* sets the final state of the dfa iff a state within the set 
     * of current states is final. *)
    method private set_fine (state: set) (dfa_ob: dfa_type): unit =
      let fs = self#is_final state in
      if not fs#is_empty then
        dfa_ob#set_final_state state
      else 
        ()

    (* set the transition for the DFA either as default if the edge is '/008'
     * or as a regular transition otherwise. *)
    method private set_trans (current: set) (e: edge) 
      (fin:set) (dfa_ob: dfa_type): unit = 
      if e = '\008' then 
        dfa_ob#set_default_transition current fin
      else 
        dfa_ob#set_transition current e fin

    (* iterate through the frontier of next states sequentially adding
     * them to the DFA *)
    method private follow_edges (current: set) (frontier: set Queue.t)
      (visited: set Queue.t) (lst: char list) (dfa_ob: dfa_type) : unit =
      match lst with
      (* we're done with this set of states *)
      | [] -> ()
      | hd::tl -> 
        (* we've already taken into account epsilon transitions by
         * going to them whenever we call next. *)
        if hd = '\007' then 
          self#follow_edges current frontier visited tl dfa_ob
        else
          (* get the next state by calling next and then evaluating
           * epsilon transitions.  Then set transitions and final *)
          let new_state = self#follow_epsilon (self#next current hd) in 
          self#set_trans current hd new_state dfa_ob;
          if not (self#in_queue new_state visited) then 
            (Queue.push new_state frontier;
             Queue.push new_state visited;
             self#set_fine new_state dfa_ob);
          self#follow_edges current frontier visited tl dfa_ob
      
    (* if the queue of sets is still populated, continue 
     * evaluating *)
    method private evaluate (dfa_ob: dfa_type) 
    (visited: set Queue.t) (frontier: set Queue.t) : unit = 
      if Queue.is_empty frontier then ()
      else 
        let current = Queue.pop frontier in 
        (* get all the transitions for this state *)
        let input_lst = self#explore_edges current in
        self#follow_edges current frontier visited input_lst dfa_ob;
        self#evaluate dfa_ob visited frontier

    (* follows epsilon transitions from a state to return the union of the 
     * new states with the original one *)
    method private follow_epsilon (state: set) : set = 
      let next = self#next state '\007' in 
      state#union next

    method construct : unit =
      let nfa_start = start_state in
      let frontier = Queue.create () in
      let dfa_start = self#follow_epsilon nfa_start in 
      Queue.push dfa_start frontier;
      let new_dfa = new DFA.dfa dfa_start in
      let visited = Queue.create () in
      self#evaluate new_dfa visited frontier;
      dfa_final <- new_dfa

    (* because the dfa type is abtracted away, we have to 
     * add this method to call its next_valid_word *)
    method next_valid_word word = dfa_final#next_valid_word word
  end
end