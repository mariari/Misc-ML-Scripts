module Fun

str_index' : (i : Nat) -> (s : String) -> {auto p : LT i (length s)} -> Char
str_index' i s {p} = assert_total $ prim__strIndex s (toIntNat i)

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

-- ltRelation : {i, n : Nat} -> LTE i n -> i <= n = True
-- ltRelation {i} {n} p =
--   let n = compare i n in
--   case n of
--     LT => ?HoleLT
--     EQ => ?HoleEQ
--     GT => ?HoleGT


getYes : (res : Dec p) -> case res of { Yes _ => p ; No _ => () }
getYes (Yes prf)   = prf
getYes (No contra) = ()

test : IO ()
test = do
  s <- getLine
  case isLTE (1 + 10) (length s) of
    Yes p => do
      let i = lteTransitive (getYes (isLTE 3 11)) p
      -- let test = ltRelation i
      -- print (str_index 2 s {p = test})
      -- we don't have to pass i
      print (str_index' 2 s {p = i})
    No  _ =>
      print "sad"
