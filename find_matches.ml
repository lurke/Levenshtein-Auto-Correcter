(*
 * find_matches.ml
 * 
 * Contains the function for finding all the matches for a given
 * word a distance away in the dictionary.
 *)

open Lev
open Matcher

(* Constructs the lookup object for the dictionary then calls the 
 * find_word_matches function to get all of the matches then 
 * prints them. *)
let run_find_matches (word: string) (dist: int) (dict: string) : unit =
  let m = construct_lookup dict in 
  let matches = find_word_matches word dist m in 
  let n = List.length matches in 
  if n > 0 then
    (print_string "There are "; print_int n; print_string " matches:\n";
    print_string (List.hd matches);
    List.iter (fun x -> print_string (", "^x)) (List.tl matches);
    print_string "\n")
  else 
    print_string "None\n"
;;