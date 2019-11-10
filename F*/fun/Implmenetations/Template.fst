module Template

open Syntax
open FStar.Tactics
open FStar.List

type n = nat
type d = nat
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

val ex1 : unit -> Lemma True
let ex1 () =
    assert_by_tactic True (fun () -> ())

val ex2 : unit -> Lemma True
let ex2 () =
    assert_by_tactic True (fun () -> (dump "Example 3"))

let tau3 () : Tac unit =
  dump "before split";
  Tactics.split ();
  dump "After split";
  // comment out smt and the next poor will fail
  smt ();
  trivial ()

val ex3 : x : nat → Lemma (x + x >= 0 /\ List.length [4;5;1] == 3)
let ex3 (x : nat) =
  assert_by_tactic (x + x >= 0 /\ List.length [4;5;1] == 3) tau3

(* but first a helper *)

let test_tactic () : Tac unit =
  dump "first"

(***** Figure out what to do *)
(* admit () gets stuck here! *)
let test (s : mult_state) =
  assert_by_tactic (ensures (fourth (List.Tot.last (eval_mult s)) == fourth s + (op_Multiply (first s) (second s))))
                   (fun () →
                     dump "here we go";
                     let binder = cur_binders () in
                     rewrite
                     dump "after intros";
                     admit ()
                   )

val eval_mult_mults_current : s:mult_state
                            → Lemma (ensures (fourth (List.Tot.last (eval_mult s)) == fourth s + (op_Multiply (first s) (second s))))
                                    (decreases %[second s; third s])
let rec eval_mult_mults_current state =
  match mult_final state with
  | true  → ()
  | false →
    let new_state = step_mult state in
    eval_mult_mults_current new_state;
    admit ()

    // definition  
    // =(1)= (_,x,y,_) <> (_,0,0,_)


val eval_mult_is_mult : n:nat
                      → d:nat
                      → Lemma (fourth (List.Tot.last (eval_mult (mult_start n d))) == op_Multiply n d)

let eval_mult_is_mult n d =
  mult_zero_start n d;
  match mult_final (mult_start n d) with
  | true -> ()
  | false -> admit ()
