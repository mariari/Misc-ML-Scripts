open Core

(* Boehm Beraducci encoding *)
type 'b lambda_bb = {
    un_lambda_bb : 'a . ('b -> 'a) -> (string -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a
  }

(* need another type if I wish to express non-folds! *)


let var x =
  { un_lambda_bb = fun d_var d_abs d_app -> d_var x }

let abs name e =
  { un_lambda_bb = fun d_var d_abs d_app -> d_abs name (e.un_lambda_bb d_var d_abs d_app) }

let app e1 e2 = {
  un_lambda_bb = fun d_var d_abs d_app ->
    d_app (e1.un_lambda_bb d_var d_abs d_app)
          (e2.un_lambda_bb d_var d_abs d_app)
}


let test_lam = abs "x" (app (app (var "y") (var "z")) (var "x"))

let test_lam_nameless = abs "x" (app (app (var 4) (var 3)) (var 0))

let view_bb e =
  let d_var n     = n in
  let d_abs s e   = String.concat ["(λ "; s; ". "; e; ")"] in
  let d_app e1 e2 = String.concat ~sep:" " [e1 ; e2] in
  e.un_lambda_bb d_var d_abs d_app


(* before I can do this, I need the positve functor encoding! *)
module Eval = struct

  type t = int lambda_bb

  let shift l ~by = 2
end

