import Data.Monoid
import Data.Sequence as S hiding (take)
import Data.Foldable
import Prelude as P
import Control.Concurrent

test = [[0,1,0]
       ,[1,1,0]
       ,[0,1,0]]

test' = S.fromList (fmap S.fromList test)

board :: Seq (Seq Integer)
board = S.fromList $ (S.fromList . apBoth zeros) <$> apBoth buffered test
  where
    zeros    = P.replicate 20 0
    buffered = P.replicate 15 (P.replicate 3 1)
    apBoth f = (f <>) . (<> f)

-- we are shifting the entire thing up one, by shifting the entire column to the left
shiftU :: Seq a -> Seq a
shiftU seq = xs |> x
  where
    (x :< xs) = S.viewl seq

-- we are shifting the entire sequence down one, by shifting the entire column to the right
shiftD :: Seq a -> Seq a
shiftD seq = x <| xs
  where
    (xs :> x) = S.viewr seq

-- we are lifting the shiftD so it shifts the entire 2d plane right
shiftR :: Seq (Seq a) -> Seq (Seq a)
shiftR = fmap shiftD

-- we are lifting the shiftU so it shifts the entire 2d plane right
shiftL :: Seq (Seq a) -> Seq (Seq a)
shiftL = fmap shiftU

-- get all permutations of 1 movements
shiftall :: Seq (Seq a) -> [Seq (Seq a)]
shiftall seqq = ($ seqq) <$> ([(shiftD .), (id .), (shiftU .)] <*> [shiftL, id, shiftR])

-- add the entire list of sequences together
-- the actions here can be done out of order and thus in parallel!!
addSame :: (Num c, Foldable t) => t (Seq (Seq c)) -> Seq (Seq c)
addSame = foldr1 (S.zipWith (S.zipWith (+)))

-- computes the next generation of alive nodes
nextGen :: (Num a, Eq a) => Seq (Seq a) -> Seq (Seq a)
nextGen = S.zipWith (S.zipWith f) =<< addSame . shiftall
  where
   f 3 _ = 1
   f 4 1 = 1
   f _ _ = 0

prettify :: (Num a, Eq a) => Seq (Seq a) -> Seq (Seq Char)
prettify  = fmap . fmap $ f
  where
    f 0 = '.'
    f 1 = 'a'


printGens :: (Eq a, Num a) => Seq (Seq a) -> IO ()
printGens = (threadDelay 100000 >> print "nextgen" >>) . traverse_ print . prettify

simulateNGens :: (Eq a, Num a) => Int -> Seq (Seq a) -> IO ()
simulateNGens n = traverse_ printGens . take n . iterate nextGen