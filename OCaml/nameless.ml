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


  let rec restore_names  ({depth ; names = {forward ; backward}} as c) = function
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

module Nl = Name_lambda

let given = Nl.create [("x",4); ("y", 3); ("z", 2); ("b", 1); ("a", 0)]

let test_lam = Abs ("x", App (Var "x", App (Var "y", Var "z")))

let is_id context lam = let numLam = Nl.remove_names context lam in
                        Nl.restore_names context numLam = lam

(*  is_id given test_lam;; => true! *)
