open Core

(** Boehm Beraducci encoding *)
type 'b lambda_bb = {
  un_lambda_bb : 'a . ('b -> 'a) -> (string -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a
}


let var x =
  { un_lambda_bb = fun d_var d_abs d_app -> d_var x }

let abs name e =
  { un_lambda_bb = fun d_var d_abs d_app -> d_abs name (e.un_lambda_bb d_var d_abs d_app) }

let app e1 e2 = {
  un_lambda_bb = fun d_var d_abs d_app ->
    d_app (e1.un_lambda_bb d_var d_abs d_app)
          (e2.un_lambda_bb d_var d_abs d_app)
}


let rec until_none f l = match f l with
  | Some x -> until_none f x
  | None   -> l

(* need another type if I wish to express non-folds! *)

module Decon = struct

  (** so the difference with this one is that it can unfold one step! *)
  type 'b lambda_decon = {
    un_decon : 'a . ('b -> 'a) -> (string -> 'b lambda_bb -> 'a) -> ('b lambda_bb -> 'b lambda_bb -> 'a) -> 'a
  }

  type 'b t = 'b lambda_decon

  let decon e =
    let d_var v = { un_decon = fun o_var o_abs o_app -> o_var v } in
    let d_abs name e = { un_decon =
                           fun o_var o_abs o_app ->
                             o_abs name (e.un_decon var abs app)
                       } in
    let d_app e1 e2 = { un_decon =
                          fun o_var o_abs o_app ->
                            o_app (e1.un_decon var abs app)
                                  (e2.un_decon var abs app)
                      } in
    e.un_lambda_bb d_var d_abs d_app

end


let test_lam = abs "x" (app (app (var "y") (var "z")) (var "x"))

let test_lam_nameless = abs "x" (app (app (var 4) (var 3)) (var 0))

let view_bb f e =
  let d_var n     = f n in
  let d_abs s e   = String.concat ["(λ "; s; ". "; e; ")"] in
  let d_app e1 e2 = String.concat ~sep:" " [e1 ; e2] in
  e.un_lambda_bb d_var d_abs d_app

let view_int = view_bb Int.to_string

let view_string = view_bb ident

(* before I can do this, I need the positve functor encoding! *)
(* note the similarities between these functions and the ones in nameless.ml! *)
module Eval = struct

  type t = int lambda_bb

  let shift l ~by =
    let rec walk count l =
      let d_app e1 e2  = app (walk count e1) (walk count e2) in
      let d_abs name e = abs name (walk (succ count) e) in
      let d_var v      = if v >= count
                         then var (v + by)
                         else var v
      in (Decon.decon l).un_decon d_var d_abs d_app
    in walk 0 l

  let subs l ~var:var' ~new_term =
    let rec walk count l =
      let d_app e1 e2  = app (walk count e1) (walk count e2) in
      let d_abs name e = abs name (walk (succ count) l) in
      let d_var v      = if v = var' + count
                         then shift new_term ~by:count
                         else var v
      in (Decon.decon l).un_decon d_var d_abs d_app
    in walk 0 l

  let is_val l =
    let d_var _   = false in
    let d_abs _ _ = true in
    let d_app _ _ = false in
    l.un_lambda_bb d_var d_abs d_app

  let shift_l e1 e2 =
    let d_var _   = None in
    let d_app _ _ = None in
    let d_abs _ e = Some (subs e ~var:0 ~new_term:e2) in
    (Decon.decon e1).un_decon d_var d_abs d_app

  let rec eval1_normal l =
    let d_app e1 e2 =
      if is_val e1
      then
        shift_l e1 e2
      else
        None
    in
    let d_var _ = None in
    let d_abs name e =
      let d_var _   = None in
      let d_abs _ _ = None in
      let d_app e1 e2 = Option.map ~f:(abs name) (eval1_normal (app e1 e2)) in
      (Decon.decon e).un_decon d_var d_abs d_app
    in
    (Decon.decon l).un_decon d_var d_abs d_app

  let eval_normal = until_none eval1_normal

  let rec eval1_value l =
    let d_var _   = None in
    let d_abs _ _ = None in
    let d_app e1 e2 =
      if is_val e1 && is_val e2 then
        shift_l e1 e2
      else if is_val e1 then
        Option.map ~f:(app e1) (eval1_value e2)
      else
        Option.map ~f:(Fn.flip app e2) (eval1_value e1)
    in
    (Decon.decon l).un_decon d_var d_abs d_app

  let eval_value = until_none eval1_value
end



let test_app_2 = app (abs "x" (var 0)) (app (abs "x" (var 0)) (abs "z" (app (abs "x" (var 0)) (var 0))))

(* # Eval.eval_normal test_app_2 |> view_int;;
 * - : Core.String.t = "(λ z. 0)"
 * # Eval.eval_value test_app_2 |> view_int;;
 * - : Core.String.t = "(λ z. (λ x. 0) 0)"
 * # test_app_2 |> view_int;;
 * - : Core.String.t = "(λ x. 0) (λ x. 0) (λ z. (λ x. 0) 0)" *)


(* # test_app_2 |> NL.remove_names given |> Eval.eval_value ;;
 * - : int lambda = Abs ("z", App (Abs ("x", Var 0), Var 0))
 * # test_app_2 |> NL.remove_names given |> Eval.eval_normal ;;
 * - : int lambda = Abs ("z", Var 0)
 * # test_app_2 |> NL.remove_names given ;;
 * - : int lambda =
App (Abs ("x", Var 0),
 App (Abs ("x", Var 0), Abs ("z", App (Abs ("x", Var 0), Var 0)))) *)
