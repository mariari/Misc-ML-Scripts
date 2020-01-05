module PlayHouse

str_index : (i : Nat) -> (s : String) -> {auto p : i < length s = True} -> Char
str_index i s {p} = assert_total $ prim__strIndex s (toIntNat i)


zeroSmallest : (n : Nat) -> Z <= n = True
zeroSmallest Z     = Refl
zeroSmallest (S n) = Refl


addRel' : (i, n, p : Nat) -> lte i n = True -> lte (p + i) (p + n) = True
addRel' n m Z     prf = prf
addRel' n m (S k) prf = rewrite addRel' n m k prf in Refl

addRel : (i, n, p : Nat) -> (i <= n = True) -> (p + i <= p + n = True)
addRel n m Z     prf = prf
addRel i n (S p) prf =
  let suc       = plusSuccRightSucc i p in
  let inductive = addRel i n p prf in
  ?HoleRel
  -- rewrite  plusSuccRightSucc i p in
  -- rewrite addRel (S i) (S n) p prf in Refl


ltRelation : {i, n : Nat} -> LTE i n -> i <= n = True
ltRelation {i = Z} {n = n} LTEZero =
  zeroSmallest n
ltRelation {i = (S i)} {n = (S n)} (LTESucc x) =
  let p = ltRelation {i} {n} x in
  let t = addRel i n 1 p in
  rewrite t in
  Refl


foo : Int -> Int -> Bool
foo n m with (succ n)
  foo _ m | 2 with (succ m)
    foo _ _ | 2 | 3 = True
    foo _ _ | 2 | _ = False
  foo _ _ | _ = False
