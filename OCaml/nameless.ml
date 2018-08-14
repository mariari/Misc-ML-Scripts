open Core

(* TAPL 6.1.5 *)
(* self made lambda *)
type 'a lambda = Var of 'a
               | Abs of string * 'a lambda
               | App of 'a lambda * 'a lambda

type context =
  { depth    : int;
    forward  : (string, int, String.comparator_witness) Map.t;
    backward : (int, string, Int.comparator_witness) Map.t;
  }

exception Foo of string

module Name_lambda : sig
  type t = context
  val create        : (string * int) list -> context
  val remove_names  : context -> string lambda -> int lambda
  val restore_names : context -> int lambda -> string lambda
end = struct

  type t = context

  let create lis =
    let f empt = List.fold_left ~f:(fun acc (key,ans) -> Map.add_exn acc key ans)
                                ~init:empt in
    { depth    = 0;
      forward  = f (Map.empty (module String)) lis;
      backward = f (Map.empty (module Int)) (List.map ~f:Tuple2.swap lis);
    }

  let rec remove_names c = function
      App (f,s)      -> App (remove_names c f, remove_names c s)
    | Abs (str,term) -> let new_depth = c.depth + 1 in
                        Abs (str, remove_names
                                    {c with depth   = new_depth;
                                            forward = Map.update c.forward
                                                                 str
                                                                 (fun _ -> -new_depth)}
                                    term)
    | Var str -> match Map.find c.forward str with
                   Some num -> Var (num + c.depth)
                 | None     -> raise (Foo String.("Variable not in map, try adding"
                                                 ^ str
                                                 ^ " to the table"))


  let rec restore_names c = function
      App (f,s)      -> App (restore_names c f, restore_names c s)
    | Abs (str,term) -> let new_depth = c.depth + 1 in
                        Abs (str, restore_names
                                    {c with depth    = new_depth;
                                            backward = Map.update c.backward
                                                                  (-new_depth)
                                                                  (fun _ -> str)}
                               term)
    | Var num -> match Map.find c.backward (num - c.depth) with
                  Some str -> Var str
                | None    -> raise (Foo String.("Variable not in map, try adding"
                                                ^ Int.to_string num
                                                ^ " to the table"))
end

let given = Name_lambda.create [("x",4); ("y", 3); ("z", 2); ("b", 1); ("a", 0)]

let test_lam = Abs ("x", App (Var "x", App (Var "y", Var "z")))

let is_id context lam = let numLam = (Name_lambda.remove_names context lam) in
                        Name_lambda.restore_names context numLam = lam

(*  is_id given test_lam;; => true! *)
