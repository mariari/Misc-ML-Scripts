module Template

open Syntax
open FStar.Tactics
open FStar.List
module Map = FStar.OrdMap


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

let test6 = fourth (List.Tot.last (eval_mult (mult_start 2 3)))

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
  | NSuperComb : name -> list name -> core_program -> node
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


type ti_stack  = list Utils.addr
let ti_heap    = Utils.heap node
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


// let compile program =
//   let sc_defs = program
//               @ Syntax.prelude_defn
//               @ Syntax.extra_prelude_defs in
//   let address_of_main = Map.sel
//   sc_defs
