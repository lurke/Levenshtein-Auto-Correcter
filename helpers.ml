(*
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, subject to the conditions
 * listed in the LAC LICENSE file. The Software is provided WITHOUT ANY 
 * WARRANTY, EXPRESS OR IMPLIED. This notice is a summary of the Masstree 
 * LICENSE file; the license in that file is legally binding. 
 *
 * helpers.ml
 *
 * Defines some functions for helping with options
 * and strings.
 *)

(* Deoptionalizes the option or raises a failure. *)
let deopt (o: 'a option): 'a =
  match o with
  | None -> raise (Failure "None")
  | Some a -> a

(* Checks if the option is a some 'a *)
let is_some (o: 'a option): bool=
  o <> None

(* Trims the white space off the beginning and end of strings *)
let string_trim (str:string) : string = 
  let rec trim_end (str: string) : string = 
    let n = String.length str in 
    if n > 0 then
      match str.[n-1] with
      | ' ' | ',' | '\012' | '\n' | '\r' | '\t'
      -> trim_end (String.sub str 0 (n-1))
      | _ -> str
    else
      ""
  in 
  let rec trim_start (str: string) : string = 
    let n = String.length str in 
    if n > 0 then 
      match str.[0] with 
      | ' ' | ',' | '\012' | '\n' | '\r' | '\t'
      -> trim_start (String.sub str 1 (n-1))
      | _ -> trim_end str
    else 
        ""
  in
  trim_start str

(* Splits the string at the given char with the char excised. *)
let string_split (c: char) (str: string) : string list =
  let rec split (str: string) (p: int) (accu: string list) =
    let n = String.length str in 
    if p < n then 
      if str.[p] = c then
        let first = String.sub str 0 p in
        let next = String.sub str (p+1) (n-p-1) in 
        split next 0 (first::accu)
      else split str (p+1) accu
    else 
      if str = "" then 
        accu
      else
        str::accu
  in
  split (string_trim str) 0 []

(* Splits the string on non-alphabetical characters*)
let strip_white (str: string) = 
    let rec split (str: string) (p:int) (accu: string list)=
      let n = String.length str in 
      if p < n then 
        let c = Char.code str.[p] in 
        if (c >= 65 && c<= 90) || (c >= 97 && c <= 122) then 
          split str (p + 1) accu 
        else 
          if p = 0 then 
            split (String.sub str 1 (n-1)) 0 accu
          else
            let first = String.sub str 0 p in 
            let next = String.sub str (p+1) (n-p-1) in 
            split next 0 (first::accu)
      else 
        if str = "" then 
          accu
        else 
          str::accu
    in
    split str 0 []


(* gets an element of a matrix*)
let get_two_dim (a: 'a array array) (i: int) (j: int) : 'a = 
  Array.get (Array.get a i) j 

(*sets the element of a matrix*)
let set_two_dim (a: 'a array array) (i:int) (j:int) (n:'a) =
  Array.set (Array.get a i) j n 
