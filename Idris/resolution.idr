module Resolution

data ℕ : Type where
  Zero : ℕ
  Suc : ℕ -> ℕ

data MyNat : Type where
  z : MyNat
  s : MyNat -> MyNat

-- foo and foo' are the same

-- Resolution works as one expects, robust under refactoring. The
-- implementation of the type does not matter, thus for consistency
-- local variables must always win the resolution game, otherwise top
-- level variables must beat local variables in pattern matching due
-- to top level constructors having that behavior. However, since
-- Idris does not match Capitals, all this is admissible, and thus we
-- can name by convention all our constructors with capital
-- names... at least the ones with 0 arguments, in which case we can
-- do an arity check.
foo : MyNat -> MyNat
foo (s z) = z
foo z     = z

foo' : MyNat -> MyNat
foo' (s s) = s
foo' a     = a

fooEqual : (x : MyNat) -> (foo' x = foo x)
fooEqual x = Refl

-- Repl output

-- λΠ> foo (s z)
-- z : MyNat
-- λΠ> foo (s (s z))
-- s z : MyNat
-- λΠ> foo z
-- z : MyNat
-- λΠ> foo (s (s (s z)))
-- s (s z) : MyNat
