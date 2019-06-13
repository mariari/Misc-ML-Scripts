(* Attempt one and transforming the bb encoding into tagless final
 * via using modules, overall this attempt is a failure, but importat
 * insight can be gained by viewing how this works (shift and subs compose!) *)


open Core

module type Lambda = sig
  type 'a repr
  val var : int -> 'a repr
  val abs : string -> 'a repr -> 'a repr
  val app : 'a repr -> 'a repr -> 'a repr
end

module type LambdaObs = sig
  include Lambda
  type 'a obs
  val observe : 'a repr -> 'a obs
end

module type LambdaNormal = sig
  include Lambda
  val add : 'a repr -> 'a repr -> 'a repr
  val substitute : 'a repr -> 'a repr -> 'a repr -> 'a repr
end

module type LambdaNormalObs = sig
  include LambdaNormal
  type 'a obs
  val observe : 'a repr -> 'a obs
end

(* Initial representation, will be used to convert back and forth in translate *)
type 'a lambda =
   | Var of 'a
   | Abs of string * 'a lambda
   | App of 'a lambda * 'a lambda


module type Monoid_Int = sig
  type t
  val (+) : t -> t -> t
  val of_int : int -> t
end

module LamInitial (M : Monoid_Int) = struct
  type 'a repr = M.t lambda
  type 'a obs  = 'a repr
  let app e1 e2 = App (e1,e2)
  let abs n e   = Abs (n,e)
  let var v     = Var (M.of_int v)
  let observe x = x

  let rec add v1 v2 = match (v1,v2) with
    | (Var v1, Var v2)             -> Var M.(v1 + v2)
    | (Abs (n, e), Abs (_, e2))    -> Abs (n, add e e2)
    | (App (e1,e2), App (e1',e2')) -> App (add e1 e1', add e2 e2')
    | _                            -> failwith "can't add two expressions of differnet types"

    let shift l ~by =
    let rec walk count = function
      | App (f, s)    -> App (walk count f, walk count s)
      | Abs (s, term) -> Abs (s, walk (succ count) term)
      | Var num       -> if num >= M.of_int count
                         then Var M.(num + by)
                         else Var num
    in walk 0 l

  let substitute l var' new_term =
    let rec walk count = function
      | App (f, s)    -> App (walk count f, walk count s)
      | Abs (s, term) -> Abs (s, walk (succ count) term)
      | Var num       -> if Var num = add var' (var count)
                         then shift new_term ~by:(M.of_int count)
                         else Var num
    in walk 0 l

end

(* This transformation will be done through trans *)
module type Trans = sig
  type 'a from
  type 'a term
  val fwd : 'a from -> 'a term
  val bwd : 'a term -> 'a from
end

(* Sadly trans is not general enough for the computation I wish to express
 * so an explicit initial to final and back is included here *)
module Convert (Lang : Lambda) (M : Monoid_Int) = struct
  include LamInitial (M)

  let f2i (x : 'a repr) = ident

  let rec i2f = function
    | App (e1, e2) -> Lang.app (i2f e1) (i2f e2)
    | Abs (n,e)    -> Lang.abs n (i2f e)
    | Var v        -> Lang.var v
end


(* This is very similar to the roll and unroll of BB encoding
 * with our intitial view we'll be using, this will be the same
 * as roll, and unroll *)
module LamTerm (X : Trans) (Lang : LambdaNormalObs with type 'a repr = 'a X.from) = struct
  open X
  type 'a repr = 'a term
  (* type 'a obs  = 'a Lang.obs *)

  let var v      = fwd (Lang.var v)
  let abs name e = fwd (Lang.abs name (bwd e))
  let app e1 e2  = fwd (Lang.app (bwd e1) (bwd e2))
  let add e1 e2  = fwd (Lang.add (bwd e1) (bwd e2))
  let substitute l v e = fwd (Lang.substitute (bwd l) (bwd v) (bwd e))

  (* let observe x = Lang.observe (bwd x) *)
end


module StringLam : LambdaObs with type 'a obs = string = struct
  type 'a repr = string
  type 'a obs  = string
  let app e1 e2 = String.concat ~sep:" " [e1 ; e2]
  let abs s e   = String.concat ["(λ "; s; ". "; e; ")"]
  let var x     = Int.to_string x
  let add x y   = String.concat ["("; x ; " " ; y ; ")"]
  let substitute l var new_term = failwith "test"
  let observe x = x
end

(* For pattern matching *)
module Ex1 (L : Lambda)  = struct
  open L
  let lam = app (abs "x" (var 0)) (app (abs "x" (var 0)) (abs "z" (app (abs "x" (var 0)) (var 0))))
end

(* let module M = Ex1(StringLam) in M.test_app_2;; *)

(* We keep this the same as in nameless ☹, but we can get the values of these
 * This is just here for reference, and to compare against Eval' which is the real final version of this
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






module Eval' (Lang : LambdaNormal) = struct
  (* Use this for introspection on converting the form
   * back and forth
   *)
  module X = struct
    type 'a from = 'a Lang.repr
    (* Define the minimal needed to succeed *)
    type 'a term =
      | Unk : 'a from -> 'a term
      | App : 'a from * 'a from -> 'a term
      | Abs : string * 'a from -> 'a term

    let fwd x = Unk x
    let bwd : type a. a term -> a from = function
      | Unk x     -> x
      | Abs (a,b) -> Lang.abs a b
      | App (a,b) -> Lang.app a b
  end
  open X
  module Eval_normal = struct
    let var = Lang.var

    let abs s = function
      | App (_,_) as t -> Abs (s, bwd t)
      | t              -> Abs (s, bwd t)

    let var x = Unk (Lang.var x)

    let app v1 v2 = match v1 with
      | Abs (_,body) ->
        Unk (Lang.substitute body (Lang.var 0) (bwd v2))
      | _ ->
        App (bwd v1, bwd v2)

    let add v1 v2 = Unk (Lang.add (bwd v1) (bwd v2))
    let substitute e1 e2 e3 = Unk (Lang.substitute e1 e2 e3)
  end

  open Lang
end

module NormalEval (Lang : LambdaNormalObs)  = struct
  module OptM = Eval' (Lang)
  include LamTerm (OptM.X) (Lang)
  include OptM.Eval_normal
end

let test =
  let module Normal = NormalEval (LamInitial (Int)) in
  let module M = Ex1 (Normal) in
  M.lam

(* test and x are now the same *)
let x =
  let module Conv = Convert (StringLam) (Int) in
  let module M = Ex1 (Conv) in
  Eval.eval_normal M.lam
  |> Conv.i2f
  |> StringLam.observe


(* This approach ultimately fails *)

(* let module M = Eval(StringLam) in M.is_val (module Ex1 : Example with type a = int);; *)
