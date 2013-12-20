(* 
 * matcher.ml
 *
 * constructs a matcher object which will return either the correct
 * word, or the next smallest one.
 *)

open Type
open Order
open States
open Helpers
open Sets

(* define the StringSet module to be used in the matcher object *)
module StringSet: SET with type elt = string = MakeSet(StringType)

(* http://camltastic.blogspot.com/2008/09/tip-read-all-lines-from-file-most.html *)
(* reads each line from a file into a list *)
let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; []
  with End_of_file ->
    close_in chan;
    List.rev !lines

(* the object type for matcher *)
class type matcher_t =
object
  inherit StringSet.setob
  method lookup: string -> string option
  method get_set: StringSet.set
end

(* defining the matcher object
 * inherits from the StringSet object then addes the lookup method *)
class matcher =
object(self)
  inherit (StringSet.setob StringSet.empty) as super
  method lookup x = 
    if super#mem x then
      Some x
    else 
      super#next x
  method get_set = super#get_set
end

(* function for constructing the matcher
 * this is accomplished by reading in each line of the file into
 * a list, then taking each entry and splits all the words
 * by different characters (see helpers: strip_white) *)
let construct_lookup file = 
  let m = new matcher in 
  let rec split (str: string) (p:int) (m: matcher_t)=
    let n = String.length str in 
    if p < n then 
      let c = Char.code str.[p] in 
      if (c >= 65 && c<= 90) || (c >= 97 && c <= 122) then 
        split str (p + 1) m 
      else 
        if p = 0 then 
          split (String.sub str 1 (n-1)) 0 m
        else 
          let first = String.sub str 0 p in 
          let next = String.sub str (p+1) (n-p-1) in 
          m#add first;
          split next 0 m
    else 
      if str = "" then 
        ()
      else 
        m#add str
  in
  List.iter (fun x -> split (String.lowercase x) 0 m) (read_file file);
  m