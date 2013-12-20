(* 
 * run.ml
 * 
 * The executable file that runs each of the parts of the project.
 * Either runs find_matches or spell check depending on command-
 * line arguments.
 *)

open Find_matches
open Spellcheck

(* From OMG Bees.
 * Parse command-line arguments. Returns the appropriate initialization
 * function to run and the current part. *)
let run () : unit =
  let usage () = print_string
    "usage: find_matches | spellcheck \n"; exit 1 in
  let usage2 () = print_string
    "usage: find_matches (word:string) (distance:int) (dictionary:string)\n"; exit 2 in
  let usage3 () = print_string
    "usage: spellcheck (file:string) (dictionary:string) \n"; exit 3 in
  if Array.length Sys.argv < 2 then usage ();
  match Sys.argv.(1) with
  | "find_matches" -> 
    if Array.length Sys.argv <> 5 then usage2 ();
      let word = Sys.argv.(2) in
      let dist = int_of_string Sys.argv.(3) in 
      let dict = Sys.argv.(4) in 
      run_find_matches word dist dict
  | "spellcheck" ->
    if Array.length Sys.argv <> 4 then usage3 ();
      let file = Sys.argv.(2) in
      let dict = Sys.argv.(3) in 
      run_spellcheck file dict 
  | _ -> usage ()
;;

run ();