import qualified Data.Vector as VB
import Data.Monoid
import Data.Char
import System.Random
import Control.Monad.State

import GHC.Prim
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive

import Data.Vector.Unboxed (freeze)
import Data.Vector.Unboxed.Mutable   as MV
import qualified Data.Vector.Unboxed as V

example :: PrimMonad m => m (V.Vector Int)
example = do
  v <- new 10
  forM_ [0..9] $ \i ->
    write v i (2*i)
  V.unsafeFreeze v

-- vector computation in IO
vecIO :: IO (V.Vector Int)
vecIO = example

-- vector computation in ST
vecST :: ST s (V.Vector Int)
vecST = example

main :: IO ()
main = do
  example >>= print
  print $ runST example

main' :: IO ()
main' = do
  vector <- MV.replicate 1 (0 :: Int)
  write vector 0 1
  ivector <- freeze vector -- changing to unsafe breaks transparency!
  print ivector
  write vector 0 2
  print ivector


vec1 :: V.Vector Int
vec1 = V.map (* 2) (V.enumFromTo 1 9)

x = VB.fromList [V.fromList [1..x] | x <- ([1..10] :: [Int])]

filter :: (Applicative f, Foldable f, Monoid (f a)) => (a -> Bool) -> f a -> f a
filter p = foldMap filt
  where filt a | p a       = pure a
               | otherwise = mempty

charvect = V.generate 20 (chr . (+ 60))

chargen :: Int -> V.Vector Char
chargen = (`V.generate` (chr . (+ 60)))
