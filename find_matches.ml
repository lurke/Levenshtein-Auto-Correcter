(* 
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, subject to the conditions
 * listed in the LAC LICENSE file. The Software is provided WITHOUT ANY 
 * WARRANTY, EXPRESS OR IMPLIED. This notice is a summary of the Masstree 
 * LICENSE file; the license in that file is legally binding.
 * 
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