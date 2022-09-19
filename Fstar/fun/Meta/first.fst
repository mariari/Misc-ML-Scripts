module 

open FStar.Tactics

let id : (#a : Type) -> a -> a =
  _ by (
  let a = intro () in
  let x = intro () in
  hyp x
  )

let test'' = id 3

let t' = `(0 + 1)
