module Template

open Syntax
open FStar.Tactics
open FStar.List
open Utils
module Map = FStar.OrdMap
module Heap = Utils.Heap

type n = nat
let  d = nat
type m = nat
type t = nat

type mult_state = (n * d * m * t)

val mult_final : mult_state -> Tot bool
let mult_final = function
  | (_, 0, 0, _) -> true
  | (_,_,_,_)    -> false

val mult_start : nat -> nat -> mult_state
let mult_start n d = (n, d, 0, 0)

val mult_zero_start : n:nat → d:nat
                    → Lemma (mult_start n d == (n,d,0,0))
let mult_zero_start n d = ()

val step_mult : (x : mult_state{not (mult_final x)}) -> Tot (y : mult_state)
let step_mult = function
  | (n, m, d, t) ->
    (match d with
     | 0 -> (n, (m - 1), n, t)
     | d -> (n, m, (d - 1), t + 1))

let fourth = function
  | (_,_,_,t) → t
let third = function
  | (_,_,m,_) → m

let second = function
  | (_,d,_,_) → d

let first = function
  | (n,_,_,_) → n

val eval_mult : (x : mult_state)
              → Tot (list mult_state)
                    (decreases (LexCons (second x) (LexCons (third x) LexTop)))
let rec eval_mult state =
  if mult_final state
  then [state]
  else state :: eval_mult (step_mult state)

// ocaml extraction lacks last for whatever reason!
// let test6 = fourth (List.Tot.last (eval_mult (mult_start 2 3)))

(***** exercise 2.2 *)


(*** Proofs *)


val step_mult_increases : x : mult_state{not (mult_final x)}
                        → Lemma (fourth x + 1 = fourth (step_mult x) \/ second x - 1 = second (step_mult x))
let step_mult_increases x = ()

val eval_mult_mults_current : s:mult_state
                            → Lemma (ensures (fourth (List.Tot.last (eval_mult s))
                                              == third s + fourth s + (op_Multiply (second s) (first s))))
                                    (decreases %[second s; third s])
let rec eval_mult_mults_current state =
  match mult_final state with
  | true  → ()
  | false → eval_mult_mults_current (step_mult state)


val eval_mult_is_mult : n:nat
                      → d:nat
                      → Lemma (fourth (List.Tot.last (eval_mult (mult_start n d))) == op_Multiply n d)
let eval_mult_is_mult n d = eval_mult_mults_current (mult_start n d)

(*** 2.3 MARK 1: Mianimal template instantian graph reducer *)



(* will be expanded in 2.6 *)
type ti_dump =
  | DummyTiDump

let initial_ti_dump = DummyTiDump

type name = string

type node =
  | Napp       : Utils.addr -> Utils.addr -> node
  | NSuperComb : name -> list name -> core_expr -> node
  | NNum       : int -> node

type total_order (a:eqtype) (f: (a -> a -> Tot bool)) =
   (forall a1 a2. (f a1 a2 /\ f a2 a1)  ==> a1 = a2)  (* anti-symmetry *)
 /\ (forall a1 a2 a3. f a1 a2 /\ f a2 a3 ==> f a1 a3)  (* transitivity  *)
 /\ (forall a1 a2. f a1 a2 \/ f a2 a1)                (* totality      *)

let string_cmp s1 s2 =  String.compare s1 s2 <= 0

(* The F* defn just calls down to OCaml, since we know comparison in OCaml is total
 * just admit it
 *)
val string_cmp_total : unit -> Lemma (total_order string string_cmp)
let string_cmp_total () = admit ()


type ti_stack   = list Utils.addr
let  ti_heap    = Heap.t node
type ti_globals = Map.ordmap name Utils.addr (string_cmp_total (); string_cmp)

// -----------------------------------------------------------------------------
// ti_stats
// -----------------------------------------------------------------------------
abstract type ti_stats = int

val ti_stat_initial : ti_stats
let ti_stat_initial = 0

val ti_stat_inc_steps : ti_stats -> ti_stats
let ti_stat_inc_steps s = s + 1


val ti_stats_get_steps : ti_stats  -> int
let ti_stats_get_steps s = s
// -----------------------------------------------------------------------------
// state
// -----------------------------------------------------------------------------

unopteq type ti_state = {
  stack   : ti_stack;
  dump    : ti_dump;
  heap    : ti_heap;
  globals : ti_globals;
  stats   : ti_stats
}

val apply_to_stats : f : (ti_stats -> ti_stats) -> ti_state -> ti_state
let apply_to_stats f state =
  {state with stats = f state.stats}


val allocate_sc : ti_heap -> core_sc_defn -> (ti_heap * (name * addr))
let allocate_sc heap (name, args, body) =
  let heap, addr = Heap.alloc heap (NSuperComb name args body) in
  heap, (name, addr)

val build_initial_heap : list core_sc_defn -> ti_heap * list (name * addr)
let build_initial_heap sc_defs = map_accum_l allocate_sc Heap.initial sc_defs

val compile : list core_sc_defn -> Prims.Tot (c_or string ti_state)
let compile program =
  let sc_defs = program
              @ Syntax.prelude_defn
              @ Syntax.extra_prelude_defs in
  let initial_heap, globals = build_initial_heap sc_defs in
  // for some reason this forgets that we proclaimed string_cmp_total ()
  // let globals_map           = Utils.list_to_map_t globals in
  let globals_map           = Utils.list_to_map globals (string_cmp_total (); string_cmp) in
  let address_of_main       = Map.select "main" globals_map in
  let initial_stack         = [address_of_main] in
  match address_of_main with
  | Some address ->
    let initial_stack = [address] in
    Right
      ({ stack = initial_stack
      ; dump  = initial_ti_dump
      ; heap  = initial_heap
      ; globals = globals_map
      ; stats   = ti_stat_initial
      })
  | None ->
    Left "No main in program"


let compile_test = compile ["main", ["x"], EVar "x"]


(**** 2.3.5 The evaluator *)

val do_admin : ti_state -> ti_state
let do_admin = apply_to_stats ti_stat_inc_steps


val is_data_node : node -> bool
let is_data_node = function
  | NNum n                      -> true
  | NSuperComb _ _ _ | Napp _ _ -> false

// maybe make this error for the None case?
val ti_final : s : ti_state{Cons? s.stack} -> bool
let ti_final state = match state.stack with
  | [sole_addr] ->
    begin match Heap.lookup state.heap sole_addr with
    | Some n -> is_data_node n
    | None   -> true
    end
  | _ :: _    -> false


val step : ti_state{Cons? s.stack} -> ti_state
let step state =
  let {stack; dump ; heap; globals; stats} = state in
  let dispatch = function
    // make NNum illegal here, thus can't be on top!
    | NNum n                  -> num_step state n
    | Napp a1 a2              -> ap_step state a1 a2
    | NSuperComb sc args body -> sc_step state sc args body
  // just map with this!
  in match Heap.lookup heap with
    | Some n -> dispatch n
    | None   -> state


let rec eval state =
  let next_state = do_admin (step state) in
  let rest_states =
    if ti_final state
    then []
    else eval next_state
  in state :: rest_states
