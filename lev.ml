(* 
 * lev.ml
 *
 * Defines the basic lev function which constructs the NFA for a word
 * a distance away based on the levenshtein algorithm.
 * Also defines the find_word_matches function which will evaluate the
 * NFA by using the powerset construction to make a DFA then evaluate that.
 *)

open Order
open Type
open Helpers
open Matcher
open Nfa
open Dfa
open States
open Freq

module ITTNFA = NFA (IntTupleType)

(* algorithm for generating a nfa from a word a given distance away.
   This algorithm uses the characters '\007' and '\008' (which represent
   "bell" and "backspace", respectively) to represent the different 
   valid alterations in the word.  In this way, "bell" represents 
   deletions, while "backspace" represents insertions and substitutions.  
   We construct our nfa using states that represent a basic, two-variable 
   coordinate system encoded as a tuple of ints.  The first coordinate 
   represents the progress of the new word so far with every state 
   with first coordinate the length of the word being an acceptable final
   state.  The second coordinate then represents the distance away from the
   initial word.
 *)
let lev (word:string) (d:int) = 
  let nfa = new ITTNFA.nfa [(0,0)] in 
  let t = String.length word in 
  let rec letters m = 
    let rec range n = 
      let c = word.[m] in 
      if n <= d then 
      (
        (* correct character goes to the right *)
        nfa#set_transition (m,n) c (m+1,n);
        (* sets a final state at the end of the row *)
        nfa#set_final_state (t,n);
        if n < d then 
        (
          (* insertion goes up *)
          nfa#set_transition (m,n) '\008' (m,n+1);
          (* deletion goes up and over *)
          nfa#set_transition (m,n) '\007' (m+1,n+1);
          (* substitution goes up and over *)
          nfa#set_transition (m,n) '\008' (m+1,n+1);

          (* entire word with another insertion goes up *)
          nfa#set_transition (t,n) '\008' (t,n+1);

          range (n+1))
        else
          range (n+1))
      else
        letters (m+1)
    in
    if m < t then 
      range 0
    else
      ()
  in
  letters 0; nfa


(* Calculates Levenshtein distance between two strings. Inspired by
 * http://en.wikipedia.org/wiki/Levenshtein_distance#Computing_Levenshtein_dista
 * nce. This solution is long code-wise but pretty fast run-time. *)

(*let rec lev_distance (s1: string) (s2: string) : int =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  if len2 > len1 then lev_distance s2 s1 else
  let mat = Array.make_matrix (len1) (len2) 0 in
     let go (i: int) (j: int) : int = 
	if i = 0 then 
	  if s1.[0] = s2.[j] then j else j + 1 else
	  if j = 0 then 
	  if s1.[i] = s2.[0] then i else i + 1 else
	let exp1 = (Helpers.get_two_dim mat (i-1) j) + 1 in
	let exp2 = (Helpers.get_two_dim mat i (j-1)) + 1 in
	let exp3a = Helpers.get_two_dim mat (i-1) (j-1) in
	let exp3b = (Helpers.get_two_dim mat (i-1) (j-1)) + 1 in
		    if s1.[i] = s2.[j] then
		      min exp1 (min exp2 exp3a) else
		      min exp1 (min exp2 exp3b) in
  for k = 0 to len1 - 1 do
    let j = ref 0 in
    while (k - !j) >= 0 && !j < len2 do
      Helpers.set_two_dim mat (k - !j) (!j) (go (k- !j) !j) ;
	j := !j + 1
    done
  done ;
    for l = 1 to len1-1 do
      let m = ref 0 in
      while (l + !m) < len2 do
	set_two_dim mat (len1-1- !m) (l+ !m) (go (len1-1- !m) (l+ !m));
	m := !m + 1
      done
    done ; Helpers.get_two_dim mat (len1-1) (len2-1)*)
let rec lev_distance (s1: string) (s2: string) : int = 
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  
  if len1 = 0 then len2 else if len2 = 0 then len1 else

    let exp1 = (lev_distance (String.sub s1 0 (len1-1)) s2) + 1 in
    let exp2 = (lev_distance  s1 (String.sub s2 0 (len2-1))) +1 in
    let exp3a = (lev_distance (String.sub s1 0 (len1-1)) 
       (String.sub s2 0 (len2-1))) + 0 in
    let exp3b = (lev_distance (String.sub s1 0 (len1-1)) 
       (String.sub s2 0 (len2-1))) + 1 in

  (* if last chars match, cost = 0 *)
  if s1.[len1 - 1] = s2.[len2 - 1] then
  min exp1 (min exp2 exp3a) else
  min exp1 (min exp2 exp3b)
;;


(* Result ranking function. Sorts the list of results in a useful order.*)
let sort_match_list (s: string) (lst: string list) (freq: bool)
    : string list = 
  if freq then
    let freq = make_freq () in 
    let tuple_list = List.map (fun t -> 
      (lev_distance t s, freq#lookup t, t)) lst in
    let sorted = List.stable_sort (fun (n1,m1,s1) (n2,m2,s2) -> 
      match m1, m2 with 
      | None,None -> compare n1 n2
      | Some _, None -> -1
      | None, Some _ -> 1
      | Some x, Some y -> (compare x y)*(-1)
    ) tuple_list in
    List.map (fun (n, m, t) -> t) sorted 
  else
    let tuple_list = List.map (fun t -> (lev_distance t s, t)) lst in
    let sorted = List.stable_sort (fun (n1,s1) (n2,s2) -> 
      compare n1 n2) tuple_list in
    List.map (fun (n, t) -> t) sorted

(* First calls lev to construct the NFA for the word a distance away.
 * Then it constructs the powerset within the given NFA. Finally, it 
 * evaluates the DFA using the next_valid_word function and the lookup
 * function for the dictionary. *)
let find_word_matches (word: string) (dist: int) (m: matcher_t): string list = 
  let nfa = lev word dist in nfa#construct;
  let rec rec_find_matches (current: string) (lst: string list): string list =
    match nfa#next_valid_word current with
    | None -> lst
    | Some w -> 
        match m#lookup w with
        | None -> lst
        | Some wn -> 
            if w = wn then 
                 rec_find_matches (w ^ " ") (w::lst)
             else rec_find_matches wn lst
  in
  sort_match_list word (rec_find_matches " " []) true
