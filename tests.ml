open Helpers
open Sets
open States
open Dfa
open Nfa
open Freq
open Matcher
open Lev
open Spellcheck

(* tests for Helpers *)
let _ = 
  assert ((deopt (Some 1)) = 1);
  assert((is_some (Some 1)) = true);
  assert((is_some None) = false);
  assert((string_trim "  hey   ") = "hey");
  assert((string_split ',' "hey,hey") = ["hey";"hey"]);
  assert((strip_white "  hey, hey  ") = ["hey";"hey"]);
  let matrix = Array.make_matrix 2 2 0 in 
  matrix.(1).(1) <- 1;
  assert((get_two_dim matrix 1 1) = 1);
  set_two_dim matrix 0 0 1;
  assert((get_two_dim matrix 0 0) = 1)

(* tests for new set function *)

module S = MakeSet (StringType)

let _ = 
  let set = new S.setob S.empty in 
  set#add "a"; set#add "b"; set#add "c";
  assert((set#next "a") = Some "b")

(* tests for DFA *)

module StringDFA = DFA (StringType)

let d = new StringDFA.dfa "";;
d#set_transition "" 'a' "a";;
d#set_transition "a" 'b' "ab";;
d#set_final_state "ab";;
let _ = 
  assert(d#next_valid_word "a" = Some "ab");
  assert(d#next_valid_word " " = Some "ab");
  assert(d#next_edge "" None = Some 'a')

module IntDFA = DFA (IntType)

let d = new IntDFA.dfa 0;;
d#set_transition 0 'a' 1;;
d#set_transition 1 'b' 2;;
d#set_final_state 2;;
let _ = 
  assert(d#next_valid_word "a" = Some "ab");
  assert(d#next_valid_word " " = Some "ab");
  assert(d#next_edge 0 None = Some 'a')

(* tests for NFA *)

module IntNFA = NFA (IntType)

let n = new IntNFA.nfa [0];;
n#set_transition 0 'a' 1;;
n#set_transition 1 'b' 2;;
n#set_final_state 2;;
n#construct;;
let _ =
  assert(n#next_valid_word " " = Some "ab")

(* tests for the frequency dictionary *)
let _ = 
  let freq = make_freq () in 
  assert(freq#lookup "the" = Some 22038615);
  assert(freq#lookup "workshop" = Some 9926)

(* tests for the matcher object *)
  let m = construct_lookup "simple.txt";;
let _ = 
  assert(m#lookup "aba" = Some "aba");
  assert(m#lookup "aba " = Some "ababa")

(* tests for lev.ml *)
let _ = 
  assert(lev_distance "hey" "heya" = 1);
  assert(lev_distance "hey" "hey" = 0);
  assert(lev_distance "hy" "heya" = 2)

(* tests for sorting by lev distance *)
let _ =
  let lst = ["aba"; "ab"; "abad"; "abacus"; "abb"] in 
  assert(sort_match_list "aba" lst false = ["aba";"ab";"abad";"abb";"abacus"])  

(* tests our find matches function *)
let _ = 
  let lst = find_word_matches "aba" 1 m in 
  assert(lst = ["aba";"abad";"abac";"ab"]);
  let lst = find_word_matches "abacus" 1 m in 
  assert(lst = ["abacus";"abacas"])

(* tests for spellchecker.ml *)
let f = construct_lookup "simple.txt";;
let d = construct_lookup "dict.txt";;
let m = new matcher;;
m#add "bfasdfa"; m#add "aba"; m#add "test";
m#add "hello";;
let _ = 
  let miss = get_misspelled f d in 
  assert(miss#is_empty = true);
  let miss = get_misspelled m d in 
  assert(miss#mem "bfasdfa" = true);
  let n = miss#fold (fun x y -> 1 + y) 0 in 
  assert(n = 1)