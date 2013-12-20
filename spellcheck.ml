(* 
 * spellcheck.ml
 * 
 * runs the spellcheck given a file to spellcheck and a dictionary 
 * to look up the words in.
 *)
open Lev
open Matcher

(* returns a set of the misspelled words.  Does this by intersecting
 * the words and the dictionary then removing all those words from
 * the original set. *)
let get_misspelled (words: matcher_t) (dict: matcher_t): StringSet.setob = 
    let miss = new StringSet.setob StringSet.empty in 
    words#fold (fun x _ -> if dict#mem x then () else miss#add x) ();
    miss
;;

(* get the suggestions for each misspelled word by constructing
 * a levenshtein automaton 2 distances away. *)
let get_suggestions (word: string) (dict: matcher_t): unit = 
    print_string ("Misspelled word: " ^ word ^ "\n");
    let lst = find_word_matches word 2 dict in 
    let n = List.length lst in 
    if n = 0 then 
        print_string "No suggestions.\n\n"
    else 
        (print_string "Did you mean: ";
        print_string (List.hd lst);
        List.iter (fun x -> print_string (", " ^ x)) (List.tl lst);
        print_string "\n\n")
;;

(* runs the spellchecking algorithm *)
let run_spellcheck (file:string) (dictfile: string) = 
  let dict = construct_lookup dictfile in 
  let words = construct_lookup file in 
  let misspelled = get_misspelled words dict in 
  let number = misspelled#fold (fun x y -> 1 + y) 0 in 
  print_string ("There are "^(string_of_int number)^" misspelled words.\n\n");
  misspelled#fold (fun x y -> get_suggestions x dict) (); flush_all()
;;