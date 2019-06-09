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

module type LambdaObs = sig
  include Lambda
  type 'a obs
  val observe : 'a repr -> 'A obs
end

(* Initial representation, will be used to convert back and forth in translate *)
type 'a lambda =
   | Var of 'a
   | Abs of string * 'a lambda
   | App of 'a lambda * 'a lambda


module LamInitial = struct
  type a = int
  type 'a repr = 'a lambda
  let app e1 e2 = App (e1,e2)
  let abs n e   = Abs (n,e)
  let var v     = Var v
end

(* This transformation will be done through trans *)
module type Trans = sig
  type 'a from
  type 'a term
  val fwd : 'a from -> 'a term
  val bwd : 'a term -> 'from
end

(* Sadly trans is not general enough for the computation I wish to express
 * so an explicit initial to final and back is included here *)
module Convert (Lang : Lambda)  = struct
  include LamInitial

  let f2i (x : 'a repr) = ident

  let rec i2f = function
    | App (e1, e2) -> Lang.app (i2f e1) (i2f e2)
    | Abs (n,e)    -> Lang.abs n (i2f e)
    | Var v        -> Lang.var v
end


(* This is very similar to the roll and unroll of BB encoding
 * with our intitial view we'll be using, this will be the same
 * as roll, and unroll *)
module LamTerm (X : Trans) (Lang : LambdaObs with type 'a repr = 'a X.from) = struct
  open X
  type a = Lang.a
  type 'a repr = 'a term
  type 'a obs  = 'a Lang.obs

  let var v      = fwd (Lang.var v)
  let abs name e = fwd (Lang.abs name (bwd e))
  let app e1 e2  = fwd (Lang.app (bwd e1) (bwd e2))

  let observe x = Lang.observe (bwd x)
end


module StringLam = struct
  type 'a repr = string
  type a = int
  let app e1 e2 = String.concat ~sep:" " [e1 ; e2]
  let abs s e   = String.concat ["(λ "; s; ". "; e; ")"]
  let var x     = Int.to_string x
end

(* For pattern matching *)
module Ex1 (L : (Lambda with type a = int))  = struct
  open L
  let lam = app (abs "x" (var 0)) (app (abs "x" (var 0)) (abs "z" (app (abs "x" (var 0)) (var 0))))
end

(* let module M = Ex1(StringLam) in M.test_app_2;; *)

(* We keep this the same as in nameless ☹, but we can get the values of these
 * functions before doing transformations that don't need to be in the initial view!
 * TODO:: Figure out how we can use Trans and LamTerm here to replace how this expression is done
 * I think we will need to extend the grammar, by adding subs and shifts, then have a reducer
 * pass, just need to treat this problem differently
*)
module Eval = struct

  type t = int lambda

  let rec until_none f l = match f l with
    | Some x -> until_none f x
    | None   -> l

  let shift l ~by =
    let rec walk count = function
      | App (f, s)    -> App (walk count f, walk count s)
      | Abs (s, term) -> Abs (s, walk (succ count) term)
      | Var num       -> if num >= count
                         then Var (num + by)
                         else Var num
    in walk 0 l

  let subs l ~var ~new_term =
    let rec walk count = function
      | App (f, s)    -> App (walk count f, walk count s)
      | Abs (s, term) -> Abs (s, walk (succ count) term)
      | Var num       -> if num = var + count
                         then shift new_term ~by:count
                         else Var num
    in walk 0 l

  let is_val = function
    | Abs _         -> true
    | Var _ | App _ -> false

  let is_vals = List.for_all ~f:is_val

  (* Evaluates the abstraction on the left argument and places t2 inside of it *)
  let shift_l v1 t2 =
    match shift v1 ~by:(-1) with
    | Abs (_,t1)    -> Some (subs t1 ~var:0 ~new_term:t2)
    | Var _ | App _ -> failwith "error, shift is only called on Abs"

  let rec eval1_normal = function
    | App (v1, t2) when is_val v1  -> shift_l v1 t2
    | Abs (str, (App (_,_) as t2)) -> Option.map (eval1_normal t2) (fun x -> Abs (str, x))
    | Var _ | Abs _ | App _        -> None


  let eval_normal = until_none eval1_normal

  let rec eval1_value = function
    | App (v1, v2) when is_vals [v2;v1] -> shift_l v1 v2
    | App (v1, t2) when is_val v1 -> Option.map (eval1_value t2) (fun t2' -> App (v1, t2'))
    | App (t1, t2)                -> Option.map (eval1_value t1) (fun t1' -> App (t1', t2))
    | Var _ | Abs _               -> None

  let eval_value = until_none eval1_value
end



(* *)
let x =
  let module Conv = Convert(StringLam) in
  let module M = Ex1(Conv) in
  Eval.eval_value M.lam
  |> Conv.i2f


(* module Eval (Lang : Lambda with type a = int) = struct
 *   (\* Use this for introspection on converting the form
 *    * back and forth
 *    *\)
 *   module X = struct
 *     type 'a from = 'a Lang.repr
 *     type 'a term =
 *       | Unk   : 'a from -> 'a term
 *       | Shift : int -> 'a term
 * 
 *     let fwd x = Unk x
 *     let bwd = function
 *       | Unk x         -> x
 *       | Shift i -> Lang.var i
 *   end
 * 
 *   open Lang
 * end *)


(* This approach ultimately fails *)

(* let module M = Eval(StringLam) in M.is_val (module Ex1 : Example with type a = int);; *)
