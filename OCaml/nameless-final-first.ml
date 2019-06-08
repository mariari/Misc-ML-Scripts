(* Attempt one and transforming the bb encoding into tagless final
 * via using modules, overall this attempt is a failure, but importat
 * insight can be gained by viewing how this works (shift and subs compose!) *)


open Core

module type Lambda = sig
  type 'a repr
  type a
  val var : a -> a repr
  val abs : string -> 'a repr -> 'a repr
  val app : 'a repr -> 'a repr -> 'a repr
end



module StringLam = struct
  type 'a repr = string
  type a = int
  let app e1 e2 = String.concat ~sep:" " [e1 ; e2]
  let abs s e   = String.concat ["(λ "; s; ". "; e; ")"]
  let var x     = Int.to_string x
end

(* For pattern matching *)

module type Decon = sig
  type 'a repr
  type 'a decon
  type a
  val d_var : a -> 'b decon
  val d_abs : string -> 'b repr -> 'a decon
  val d_app : 'b repr -> 'b repr -> 'a decon
end


(* A very bad way of handling trying to run the data
 * It works, but is a rather odd encoding *)
module type Example = functor (L : Lambda with type a = int) ->
sig
  val lam : L.a L.repr
end

module Ex1 (L : (Lambda with type a = int))  = struct
  open L
  let lam = app (abs "x" (var 0)) (app (abs "x" (var 0)) (abs "z" (app (abs "x" (var 0)) (var 0))))
end

(* let module M = Ex1(StringLam) in M.test_app_2;; *)

(* A very poor way of doing this I think?, trying to reconsturct pattern matching this way *)
module Eval (Lang : Lambda with type a = int) = struct
  open Lang

  let shift l ~by =
    let module Shift = struct
      type 'b repr = int -> 'b Lang.repr
      type a = int
      let app e1 e2 count = app (e1 count) (e2 count)
      let abs n e count   = abs n (e (succ count))
      let var v count =
        if v >= count
        then var (v + by)
        else var v
    end in
    let module Run = (val l : Example) (Shift) in
    Run.lam 0

  let subs l ~var:var' ~new_term =
    let module Subs = struct
      type 'b repr = int -> 'b Lang.repr
      type a = int

      let app e1 e2 count = app (e1 count) (e2 count)
      let abs n e count   = abs n (e (succ count))

      let var v count =
        if v = var' + count
        then shift new_term ~by:count
        else var v
    end in
    let module Run = (val l : Example) (Subs) in
    Run.lam 0

  let is_val l =
    let module Run = (val l : Example)
        (struct
          type a = int
          type 'b repr = bool
          let var _   = false
          let abs _ _ = true
          let app _ _ = false
        end) in
    Run.lam

  (* let shift_l e1 e2 =
   *   let module Run = (val e1 : Example)
   *       (struct
   *         type a = int
   *         type 'a repr = int option
   *         let var _ = None
   *         let app _ _ = None
   *         let abs _ e = Some (subs e ~var:0 ~new_term:e2)
   *       end) in
   *   Run.lam *)
end


(* This approach ultimately fails *)

(* let module M = Eval(StringLam) in M.is_val (module Ex1 : Example with type a = int);; *)
