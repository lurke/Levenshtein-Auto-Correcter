(*
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, subject to the conditions
 * listed in the LAC LICENSE file. The Software is provided WITHOUT ANY 
 * WARRANTY, EXPRESS OR IMPLIED. This notice is a summary of the Masstree 
 * LICENSE file; the license in that file is legally binding.
 * 
 * dfa.ml
 * 
 * Defines the DFA module functor/object constructor given
 * a state type
 *)

open Type
open Helpers
open Twothree
open Sets

(* define DFA a module/object mix which constructs a dictionary and set
   that are used within the class dfa within which requries a start state
   the type of the start state must match state and D.key *)
module DFA (State: STATE):
  DFA_T with type state = State.t  =
struct
  type state = State.t
  type edge = char 
  (* define a dictionary for transitions *)
  module Trans = 
    MakeDict(struct
              type key = state
              type value = (char * state ) list 
              let compare = State.compare
              let string_of_key = State.string_of_t
              let string_of_value v = ""
            end)
  (* define a dictionary for default transitions *)
  module Def = 
    MakeDict(struct
              type key = state
              type value = state
              let compare = State.compare
              let string_of_key = State.string_of_t
              let string_of_value v = ""
            end)
  module S: SET with type elt = State.t = MakeSet(State)
  class dfa (start: state) =
  object(self)
    (* Instance Variables *)
    val start_state = start
    val mutable final_states = new S.setob S.empty
    val mutable defaults = new Def.dict
    val mutable transitions = new Trans.dict

    (* Methods *)
    method set_final_state (s:state): unit = final_states#add s

    method is_final (s:state) : bool = final_states#mem s 

    method set_default_transition s n = defaults#add s n

    method set_transition (s: state) (e:edge) (n:state): unit = 
      match transitions#lookup s with
      | None -> transitions#add s [(e,n)]
      | Some xs -> transitions#add s ((e,n)::xs)

    method next_state (s:state) (e: edge): state option = 
      match transitions#lookup s with
      | None -> defaults#lookup s
      | Some xs -> let r = List.fold_right 
                      (fun x y -> let (a,b) = x in 
                        if a = e then Some b else y) xs None in 
                    if is_some r then 
                      r 
                    else
                      defaults#lookup s

    (* helper to find the next smallest element in the list *)
    method private get_next_elm (e:edge) (lst: edge list): edge option =
      match lst with
      | [] -> None
      | x :: xs -> if compare e x <= 0 then Some x else self#get_next_elm e xs

    method next_edge (s:state) (e: edge option): edge option = 
      let en = ref ' ' in 
      if is_some e then 
        en := Char.chr ((Char.code (deopt e)) + 1);
      let trans = transitions#lookup s in 
      if is_some (defaults#lookup s) then
        Some !en
      else if is_some trans then 
        let edges = List.sort compare
        (List.map (fun x -> let (a,_) = x in a) 
          (deopt trans)) in
        self#get_next_elm !en edges
      else 
        None 

    (* builds a stack for next_valid_word of 
     * (cumulative word, current, current edge) *)
    method private build_stack (str:string) (s:state) (e:edge) (n:int) lst = 
      let lst = (String.sub str 0 n, Some s, Some e)::lst in
      match self#next_state s e with 
      | None -> (str, None, None)::lst
      | Some next ->  
          if n < String.length str - 1 then
            self#build_stack str next (String.get str (n+1)) (n+1)
              lst
          else
            (str,Some next, None)::lst

    (* walks down the list checking if each word has any valids next *)
    method private walk lst: string option= 
      match lst with
      | [] -> None
      | (word, Some s, eopt)::tl -> 
         (match self#next_edge s eopt with
          | None -> self#walk tl
          | Some e ->
              let word = word ^ (Char.escaped e) in 
              let state = deopt (self#next_state s e) in 
              if self#is_final state then
                Some word
              else
                self#walk ((word,Some state, None)::lst))
      | (_,_,_)::tl -> self#walk tl

    method next_valid_word (str:string): string option= 
      let lst = self#build_stack str start_state (String.get str 0) 0 [] in 
      let (w, s,_) = List.hd lst in 
      if is_some s && self#is_final (deopt s) then
          Some w
      else 
        self#walk lst
  end
end