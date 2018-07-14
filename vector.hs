import qualified Data.Vector as V
import Data.Monoid
import Data.Char
import System.Random
import Control.Monad.State


vec1  = V.fromList [1..19]


x = V.fromList [V.fromList [1..x] | x <- [1..10]]


filter :: (Applicative f, Foldable f, Monoid (f a)) => (a -> Bool) -> f a -> f a
filter p = foldMap filt
  where filt a | p a       = pure a
               | otherwise = mempty

charvect = V.generate 20 (chr . (+ 60))

chargen :: Int -> V.Vector Char
chargen = (`V.generate` (chr . (+ 60)))
