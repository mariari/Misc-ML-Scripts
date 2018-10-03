(* open Core *)

type symbol = string

type expr = Nil
          | StringLit  of string
          | IntLit     of int
          | BoolLit    of bool
          | FloatLit   of float
          | Identifier of symbol
          | IfThen     of expr * expr
          | IfThenElse of expr * expr * expr
          | LetExp     of declare list * expr
          | Funcall    of symbol * expr list

and declare = Var of symbol * expr
            | Fun of symbol * symbol list * expr

and ret = String of string
        | Int    of int
        | Float  of float
        | Bool   of bool
        | Func   of symbol * string list * expr
        | Null

exception Foo of string

module StateMap = Map.Make(String)

let add_new_state xs = StateMap.empty :: xs

let rec eval state = function
  | Nil                     -> Null,         state
  | BoolLit    b            -> Bool   b,     state
  | StringLit  str          -> String str,   state
  | IntLit     int          -> Int    int,   state
  | FloatLit   float        -> Float  float, state
  | IfThen     (pred, t)    -> eval_if state pred t Nil
  | IfThenElse (pred, t, e) -> eval_if state pred t e
  | LetExp     (exs, ex)    -> eval_let state exs ex
  | Identifier (var)        -> eval_ident state var
  | Funcall    (fn, args)   -> Null, state

and eval_bool state pred = match eval pred state with
  | (Bool b, s) -> b,     s
  | (Null,   s) -> false, s
  | (_,      s) -> true,  s

and eval_if s p t e = match eval_bool p s with
  | true, s  -> eval s t
  | false, s -> eval s e

and eval_let s exs ex =
  let rec add_to_state s = function
    | [] ->
       s
    | Var (var, value) :: xs ->
       let v, s' = eval s value in
       add_to_state List.(StateMap.add var v (hd s') :: tl s') xs
    | Fun (var, binds, exp) :: xs ->
       StateMap.add var
                    (Func (var, binds, exp))
                    (List.hd s)
       :: List.tl s in
  let let_state           = add_to_state (add_new_state s) exs in
  let value, (_ :: state) = eval let_state ex [@@warning "-8"] in
  value, state


and eval_ident s var = match List.filter (StateMap.mem var) s with
    | x :: _ -> StateMap.find var x, s
    | _      -> raise @@ Foo ("The Variable " ^ var ^ " is unbound")

let letexpr = LetExp ([Fun ("hello", ["sutf"], (IntLit 2))], Identifier "hello")
