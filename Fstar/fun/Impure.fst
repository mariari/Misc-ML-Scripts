module Impure

open FStar
open FStar.All


let my_arr =
  let my_arr = Array.of_seq (Seq.of_list [1;2;3;4;5]) in
  ignore (Array.index my_arr 0);
  my_arr

let operation () =
  Array.index my_arr 0


let foo =
  let my_arr = Array.create 10 5 in
  Array.index my_arr 0


val main : x : int -> ST.ST (list int)
                    (fun h -> Array.contains h my_arr)
                    (fun h0 v h1 -> True)
let main x =
  if 0 < Array.length my_arr
  then let original_index = operation () in
       let index = Array.index my_arr 0 in
       Array.upd my_arr 0 (index + 10);
       let index = Array.index my_arr 0 in
       Array.upd my_arr 0 (index + 10);
       [operation (); original_index]
  else []
