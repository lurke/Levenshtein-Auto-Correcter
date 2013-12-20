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
