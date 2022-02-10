
module Impure

open FStar

val my_arr : Array.array int

val operation
    :  unit
    -> FStar.ST.ST int
        (fun h -> Array.contains h my_arr
                /\ 0 < Seq.Base.length (Array.sel h my_arr))
        (fun h0 v h1 ->
            0 < Seq.Base.length (Array.sel h0 my_arr) /\ h0 == h1 /\
            v == Seq.Base.index (Array.sel h0 my_arr) 0)

