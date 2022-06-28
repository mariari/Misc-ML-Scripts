module Resolution where

  module Important where
    id : (a : Set) → a → a
    id a X = X

  data ℕ : Set where
    z   : ℕ
    sic : ℕ → ℕ

  zero : ℕ
  zero = z

  -- we should not export our consturcotrs due to the bijection and
  -- refactoring could break others code, even if we intended other
  -- users to never use our consturcotrs but export it for a test
  -- module. This can cause long term issues.

  data Foo : Set where
    Bar : Foo

  foo : ℕ -> ℕ
  foo (sic BAZ) = BAZ
  foo z         = zero
