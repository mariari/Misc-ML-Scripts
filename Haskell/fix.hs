module Fix where

import Data.Function

-- | @fixList@ finds the fixed point of the listified version of @iter@
-- in which the nth application of the function is saved in the nth
-- slot of the list. The analytical fixed point can be formed by
-- finding values below a certain predicate checker where the values
-- are similar enough for whatever that domain may be.
fixList :: (t -> t) -> t -> [t]
fixList iter =
  fix (\loop z -> z : loop (iter z))

-- | @belowThreshold@ is similar to Sussman's definition of
-- good-enough? in SICP.
belowThreshold :: (Ord p, Fractional p) => [p] -> p
belowThreshold (x : y : xs)
  | abs ( x - y ) < 0.00001 = x
  | otherwise               = belowThreshold xs


-- | @fixA@ is the analytical fix point of a function. It achieves this
-- by turning the analytical function into fixed point list
fixA :: (Ord c, Fractional c) => (c -> c) -> c -> c
fixA iter = belowThreshold . fixList iter

averageDamp :: Fractional a => a -> a -> a
averageDamp x y = (x + y) / 2

-- Using the Average dampening method
sqrt :: (Ord a, Fractional a) => a -> a
sqrt x = fixA (\y -> averageDamp y (x / y)) 1
