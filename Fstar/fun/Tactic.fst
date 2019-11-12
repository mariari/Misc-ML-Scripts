module Tactic

open FStar.Tactics
open FStar.List

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
