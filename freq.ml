(* 
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, subject to the conditions
 * listed in the LAC LICENSE file. The Software is provided WITHOUT ANY 
 * WARRANTY, EXPRESS OR IMPLIED. This notice is a summary of the Masstree 
 * LICENSE file; the license in that file is legally binding.
 *
 *)

open Order
open Twothree

module D = 
struct
  type key = string
  type value = int
  let compare x y = 
    let n = compare x y in
    if n < 0 then 
      Less
    else if n = 0 then
      Eq
    else
      Greater
  let string_of_key x = x
  let string_of_value = string_of_int
end

module Dict = MakeDict (D)


let make_freq (): Dict.dict = 
  let chan = open_in "frequencies.csv" in
  let dict = new Dict.dict in 
  try
    while true; do
      let lst = Helpers.string_split ',' (input_line chan) in
      match lst with 
      | x::y::[] -> dict#add y (int_of_string x)
      | _ -> ()
    done; dict
  with End_of_file ->
    close_in chan;
    dict
