module First

open FStar.Tactics
open FStar.Tactics.Derived


(**** Meta I came up with myself *)

let add_1_generator () : Tac unit =
  let x = intro () in
  let tm = (`(`#x + `@1)) in
  apply tm

val add_1 : int -> int
let add_1 = _ by (add_1_generator ())



(**** Meta I found online *)


val id (#a : Type) : a -> a
let id  =
  _ by (
  let a = intro () in
  let x = intro () in
  hyp x
  )

let test'' = id 3

let t' x = `(0 + 1 ,x)
let t'' = t' 2

let f (x y : int) : int = _ by (exact (`42))

let mk_add () : Tac unit =
  let x = intro () in
  let y = intro () in
  apply (`(+));
  exact y;
  exact x

let add : int -> int -> int =
  _ by (mk_add ())


type t1 =
  | A : int -> int -> t1
  | B : string -> t1
  | C : t1 -> t1

// let t1_print : t1 -> string = _ by ( ())

(* interesting to note is that we can `@z here *)
let _ = assert True
            by (let tm = (`(1 + `@(1))) in
                let z = 16 in
                let x = `16 in
                let tm2 = `(1 + `@(z)) in
                let tm3 = `(1 + `#(x)) in
                debug ("tm = " ^ term_to_string tm);
                debug ("tm2 = " ^ term_to_string tm2);
                debug ("tm3 = " ^ term_to_string tm3);
                let ty = tc (cur_env ()) tm in
                debug ("ty = " ^ term_to_string ty);
                let ty2 = tc (cur_env ()) tm2 in
                debug ("ty2 = " ^ term_to_string ty2);
                let ty3 = tc (cur_env ()) tm3 in
                debug ("ty3 = " ^ term_to_string ty3);
                ())
