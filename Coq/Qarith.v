Require Coq.QArith.QArith_base.

Import Coq.QArith.QArith_base.

Theorem div_same_plus_m : forall n m,
    ~ n == 0
    -> (n / n) + m == 1 + m.
Proof.
  intros n m H.
  apply Qmult_inv_r in H.
  rewrite Qplus_inj_r.
  apply H.
Qed.
