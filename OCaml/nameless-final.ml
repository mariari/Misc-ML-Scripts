(* Attempt one and transforming the bb encoding into tagless final
 * via using modules, overall this attempt is a failure, but importat
 * insight can be gained by viewing how this works (shift and subs compose!) *)


open Core

module type Lambda = sig
  type 'a repr
  val int : int -> int repr
  val add : int repr -> int repr -> int repr

  val lam : ('a repr -> 'b repr) -> ('a -> 'b) repr
  val app : ('a -> 'b) repr -> 'a repr -> 'a repr
end

module type LambdaObv = sig
  include Lambda
  type 'a obs
  val observe : 'a repr -> 'a obs
end

module Ex1 (L : LambdaObv)  = struct
  open L
  let lam = observe @@
    app (lam (fun x -> x)) (app (lam (fun x -> x)) (lam (fun z -> (app (lam (fun x -> x)) z))))
end


type str = {unStr : int -> string}

let str x = {unStr = x}

module StringLam : (LambdaObv with type 'a obs = str) = struct
  type 'a repr = str
  type 'a obs  = str
  let int x = Int.to_string x |> const |> str

  let add e1 e2 =
    str @@ fun h ->
    String.concat ["("; e1.unStr h; " + "; e2.unStr h; ")"]

  let app e1 e2 =
    str @@ fun h ->
    String.concat ~sep:" " [e1.unStr h ; e2.unStr h]

  let lam e =
    str @@ fun h ->
    String.concat ["(λ "; Int.to_string h; ". "
                  ; (e (int h)).unStr (succ h); ")"]

  let observe = ident
end

let view e = e.unStr 0
