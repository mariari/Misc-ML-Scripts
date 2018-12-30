open Core

module Symbol : sig
  type t [@@deriving hash]
  include Comparator.S with type t := t
  val of_string : string -> t
  val to_string : t -> string

end = struct
  module T = struct
    type t = string [@@deriving compare, hash, sexp]
  end
  include T
  include Comparator.Make(T)
  let of_string t = t
  let to_string t = t
end

type expr = Nil
          | StringLit  of string
          | IntLit     of int
          | BoolLit    of bool
          | FloatLit   of float
          | Identifier of Symbol.t
          | IfThen     of expr * expr
          | IfThenElse of expr * expr * expr
          | LetExp     of declare list * expr
          | Funcall    of Symbol.t * expr list

and declare = Var of Symbol.t * expr
            | Fun of Symbol.t * Symbol.t list * expr

and ret = String of string
        | Int    of int
        | Float  of float
        | Bool   of bool
        | Func   of Symbol.t * Symbol.t list * expr
        | Null

exception Foo of string

let add_new_state xs = Map.empty (module Symbol) :: xs

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
       (match eval s value with
        | v, []           -> assert false
        | v, s1 :: s_rest -> add_to_state (Map.change s1 var (Some v |> const) :: s_rest) xs)
    | Fun (var, binds, exp) :: xs ->
       let s1,s_rest =
         match s with
         | [] -> assert false
         | s :: ss -> s, ss
       in
       Map.change s1
                  var
                  (Some (Func (var, binds, exp)) |> const)
       :: s_rest
  in
  let let_state = add_to_state (add_new_state s) exs in
  match eval let_state ex with
  | _, []             -> assert false
  | value, _ :: state -> (value, state)


and eval_ident s var =
  match List.filter ~f:(Fn.flip Map.mem var) s with
  | m :: _ -> Map.find_exn m var , s
  | _      -> raise @@ Foo ("The Variable " ^ Symbol.to_string var ^ " is unbound")

let letexpr = LetExp ([Fun (Symbol.of_string "hello"
                           , [Symbol.of_string "stuff"], (IntLit 2))]
                     , Identifier (Symbol.of_string "hello"))
