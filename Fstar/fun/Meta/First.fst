module First

open FStar.Tactics

val id (#a : Type) : a -> a
let id  =
  _ by (
  let a = intro () in
  let x = intro () in
  hyp x
  )

let test'' = id 3

let t' = `(0 + 1)
