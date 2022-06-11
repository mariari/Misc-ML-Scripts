Require Coq.Lists.List.

Import List.ListNotations.

Local Open Scope list_scope.

Require Export ZArith_base.

Import BinIntDef.Z.

Local Open Scope Z_scope.

Inductive move : Type
  := Left
  | Right
  | Up
  | Down.

Definition eq_dec (x y : move) : {x = y} + {x <> y}.
Proof.
 decide equality.
Defined.


Compute move_rect.

Definition move_set := list move.

Inductive base_state : Type :=
  { x : Z;
    y : Z;
  }.

Compute (x {| x := 3; y := 4 |}).


(*  | 0,0 | 0,1 | 0,2  | 0,3  | 0,4   | 0,5 | *)
(*  |-----+-----+------+------+-------+-----| *)
(*  | 1,0 |     |      |      |       |     | *)
(*  | 2,0 |     |      |  up  |       |     | *)
(*  | 3,0 |     | left | here | right |     | *)
(*  | 4,0 |     |      | down |       |     | *)
(*  | 5,0 |     |      |      |       |     | *)
Definition app_inst (s : base_state) (move : move) : base_state :=
  match move with
  | Left  => {|x := x s;     y := y s - 1|}
  | Right => {|x := x s;     y := y s + 1|}
  | Up    => {|x := x s - 1; y := y s|}
  | Down  => {|x := x s + 1; y := y s|}
  end.

Definition run (s : base_state) (moves : move_set) := List.fold_left app_inst moves s.

Compute (List.count_occ Z.eq_dec [2;3;4] 3).

Compute Z.eqb.

Definition left  (xs : move_set) : nat := List.count_occ eq_dec xs Left.
Definition right (xs : move_set) : nat := List.count_occ eq_dec xs Right.
Definition up    (xs : move_set) : nat := List.count_occ eq_dec xs Up.
Definition down  (xs : move_set) : nat := List.count_occ eq_dec xs Down.

Definition default_spot := {| x := 0; y := 0|}.

(* prove the more general case first *)
Lemma x_bounded : forall (moves : move_set) (state : base_state),
    left moves = right moves -> y state = y (run state moves).
Proof.
  intros m .
  induction m as [| m' IHm'].
  - simpl.
    reflexivity.
  - intros s impl.
    destruct m' eqn:E.
    (* Figure out how to remove this repeat call!!! *)
    (* Also simplify this proof... kind of a mess of unfodling *)
    Focus 3.
    simpl;
    unfold left in impl;
    unfold right in impl;
    simpl in impl;
    unfold left in IHIHm';
    unfold right in IHIHm';
    rewrite <- impl in IHIHm';
    simpl in IHIHm';
    rewrite <- IHIHm' at 1;
    reflexivity.
    (* Figure out how to remove this repeat call!!! *)
    Focus 3.
    simpl;
    unfold left in impl;
    unfold right in impl;
    simpl in impl;
    unfold left in IHIHm';
    unfold right in IHIHm';
    rewrite <- impl in IHIHm';
    simpl in IHIHm';
    rewrite <- IHIHm' at 1;
    reflexivity.
    + simpl.
      simpl in impl.
      unfold left in impl.
      unfold right in impl.
      simpl in impl.
      unfold left in IHIHm'.
      unfold right in IHIHm'.
admit.

