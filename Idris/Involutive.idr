module Involutive

data MyList : Type -> Type where
  Nil : MyList elem
  (::) : elem -> MyList elem -> MyList elem

append : List a -> List a -> List a
append Nil ys = ys
append (x :: xs) ys = x :: append xs ys

rev : List a -> List a
rev Nil       = Nil
rev (x :: xs) = append (rev xs) (x :: Nil)

snoc : List a -> a -> List a
snoc xs x = append xs (x :: Nil)

snoc_cons : (l : List a) -> (h : a) -> rev (snoc l h) = h :: rev l
snoc_cons Nil       _ = Refl
snoc_cons (x :: xs) h =
  let foo = snoc_cons xs h in
  rewrite foo in
  Refl

rev_involutive : (l : List a) -> rev (rev l) = l
rev_involutive Nil = Refl
rev_involutive (x :: xs) =
  rewrite snoc_cons (rev xs) x in
  rewrite rev_involutive xs in
  Refl

involutive_injective
  :  {t : Type}
  -> {f : t -> t}
  -> {a, b : t}
  -> ((z : t) -> f (f z) = z)
  -> f a = f b
  -> a = b
involutive_injective {b} inv app =
  rewrite sym (inv a) in
  rewrite app in
  inv b

rev_injective : (l1 : List a) -> (l2 : List a) -> rev l1 = rev l2 -> l1 = l2
rev_injective xs ys prf =
  involutive_injective rev_involutive prf
