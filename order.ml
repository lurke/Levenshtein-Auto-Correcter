(* 
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, subject to the conditions
 * listed in the LAC LICENSE file. The Software is provided WITHOUT ANY 
 * WARRANTY, EXPRESS OR IMPLIED. This notice is a summary of the Masstree 
 * LICENSE file; the license in that file is legally binding.
 * 
 *)
(* The type order is used for comparison operations *)
type order = Less | Eq | Greater ;;

let string_compare x y = 
  let i = String.compare x y in
    if i = 0 then Eq else if i < 0 then Less else Greater ;;

let int_compare x y = 
  let i = x - y in 
    if i = 0 then Eq else if i < 0 then Less else Greater ;;

let char_compare x y =
  let comp = Char.compare x y in
  if comp = 0 then Eq
  else if comp > 0 then Greater
  else Less
