open Core

(* TAPL 6.1.5, da brujin indicies! *)
(* self made lambda *)
type 'a lambda =
     Var of 'a
   | Abs of string * 'a lambda
   | App of 'a lambda * 'a lambda

type name_scheme = {
    forward  : (string, int, String.comparator_witness) Map.t;
    backward : (int, string, Int.comparator_witness) Map.t;
  }

type context = {
    depth : int;
    names : name_scheme
  }

exception Foo of string

let rec until_none f l = match f l with
    Some x -> until_none f x
  | None   -> l

module Name_lambda : sig
  type t = context
  val create        : (string * int) list -> t
  val remove_names  : t -> string lambda -> int lambda
  val restore_names : t -> int lambda -> string lambda
end = struct

  type t = context

  let create lis =
    let f empt = List.fold_left ~f:(fun acc (key,ans) -> Map.add_exn acc key ans)
                                ~init:empt in
    { depth = 0;
      names = { forward  = f (Map.empty (module String)) lis;
                backward = f (Map.empty (module Int)) (List.map ~f:Tuple2.swap lis);
      }}

  let rec remove_names ({depth ; names = {forward ; backward}} as c) = function
      App (f,s)      -> App (remove_names c f, remove_names c s)
    | Abs (str,term) -> let new_depth = depth + 1 in
                        Abs (str, remove_names
                                    {depth = new_depth;
                                     names = {backward = backward;
                                              forward  = Map.update forward
                                                                    str
                                                                    (fun _ -> -new_depth)}}
                                    term)
    | Var str -> match Map.find forward str with
                   Some num -> Var (num + depth)
                 | None     -> raise (Foo String.("Variable not in map, try adding"
                                                  ^ str
                                                  ^ " to the table"))


  let rec restore_names ({depth ; names = {forward ; backward}} as c) = function
      App (f,s)      -> App (restore_names c f, restore_names c s)
    | Abs (str,term) -> let new_depth = depth + 1 in
                        Abs (str, restore_names
                                    {depth  = new_depth;
                                     names = {forward  = forward;
                                              backward = Map.update backward
                                                                    (-new_depth)
                                                                    (fun _ -> str)}}
                                    term)
    | Var num -> match Map.find backward (num - depth) with
                  Some str -> Var str
                | None     -> raise (Foo String.("Variable not in map, try adding"
                                                 ^ Int.to_string num
                                                 ^ " to the table"))
end

module NL = Name_lambda

module Eval = struct

  type t = int lambda

  let shift d l =
    let rec walk count = function
        App (f, s)    -> App (walk count f, walk count s)
      | Abs (s, term) -> Abs (s, walk (count + 1) term)
      | Var num       -> if num >= count
                         then Var (num + d)
                         else Var num
    in walk 0 l

  let subs var new_term l =
    let rec walk count = function
        App (f, s)    -> App (walk count f, walk count s)
      | Abs (s, term) -> Abs (s, walk (count + 1) term)
      | Var num       -> if num = var + count
                         then shift count new_term
                         else Var num
    in walk 0 l

  let is_val = function
      Abs _ -> true
    | _     -> false

  let is_vals = List.for_all ~f:is_val

  (* Evaluates the abstraction on the left argument and places t2 inside of it *)
  let shift_l v1 t2 = let Abs(_,t1) = shift (-1) v1 [@@warning "-8"] in
                      Some (subs 0 t2 t1)

  let rec eval1_normal = function
      App (v1, t2) when is_val v1  -> shift_l v1 t2
    | Abs (str, (App (_,_) as t2)) -> Option.map (eval1_normal t2) (fun x -> Abs (str, x))
    | _                            -> None

  let eval_normal = until_none eval1_normal

  let rec eval1_value = function
      App (v1, v2) when is_vals [v2;v1] -> shift_l v1 v2
    | App (v1, t2) when is_val v1 -> Option.map (eval1_value t2) (fun t2' -> App (v1, t2'))
    | App (t1, t2)                -> Option.map (eval1_value t1) (fun t1' -> App (t1', t2))
    | _                           -> None

  let eval_value = until_none eval1_value
end


(* Testing code *)

let given = NL.create [("x", 4); ("y", 3); ("z", 2); ("b", 1); ("a", 0)]

let test_lam = Abs ("x", App (App (Var "y", Var "z"), Var "x"))

let is_id context lam = let numLam = NL.remove_names context lam in
                        NL.restore_names context numLam = lam

(*  is_id given test_lam;; => true! *)

let test_app = App (test_lam, Var "y")

let identity = Abs ("x", Var "x")

let test_app_2 = App (identity, App (identity, Abs ("z", App (identity, Var "z"))))

(* (Eval.eval_value (NL.remove_names given test_app_2));; *)
