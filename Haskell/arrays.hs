{-# LANGUAGE TypeOperators, FlexibleContexts #-}
import Data.Array.Repa as R
import Data.Vector.Unboxed.Base
import Data.Array.Repa.Algorithms.Matrix
import Data.Monoid
import Data.Char

x = fromListUnboxed (Z :. (10 :: Int)) ([1..10] :: [Double])

--keypad :: (Unbox a, num a, Enum a) => Array U ((Z :. Int) :. Int) a
keypad :: Array U ((Z :. Int) :. Int) Int
keypad = fromListUnboxed (Z :. (3 :: Int) :. (3 :: Int)) [1..9]


-- mapping creates a D-elayed array inefficient when the data is needed multiple times
xy :: Array D ((Z :. Int) :. Int) Int
xy = R.map (^2) keypad



-- DIM2 == ((Z :. Int) :. Int)

-- unbox it sequentially
zz :: Array U ((Z :. Int) :. Int) Int
zz = R.computeUnboxedS xy

-- unbox it in parallel... adds a monad to it, though :(
zp :: Monad m => m (Array U DIM2 Int)
zp = R.computeUnboxedP xy

-- accessing the last slot
-- ! is infix index
z = xy ! (Z :. 2 :. 2)

zsad :: Monad m => m Int
zsad = (! (Z :. 2 :. 2)) <$> zp

move :: Num a => Char -> (a,a) -> ((a,a) -> (a,a) -> (a,a)) -> (a,a)

move mov (x,y) restrict = restrict new (x,y)
  where new = case toUpper mov of
          'U' -> (x - 1, y)
          'D' -> (x + 1, y)
          'L' -> (x, y - 1)
          'R' -> (x, y + 1)
          _   -> (x,y)

threeWraper :: (Num a, Ord a) => (a,a) -> (a,a) -> (a,a)
threeWraper old new = threeByThreeRes old

threeByThreeRes :: (Num a, Ord a) => (a,a) -> (a,a)
threeByThreeRes (x,y) = (m x, m y)
  where m = min 2 . max 0

getnum :: Source r e => (Int, Int) -> Array r DIM2 e -> e
getnum (x,y) arr = arr ! (Z :. x :. y)

grabSlot :: Source r e => (Int, Int) -> Char -> Array r DIM2 e -> e
grabSlot tul mov = getnum (move mov tul threeWraper)


-- grabSlot (2,2) 'U' keypad


--(Z :. a :. b) = extent keypad
a = fromListUnboxed (Z :. 4 :. 4) [1..16] :: Array U DIM2 Int
-- computeUnboxedS (slice a (Any :. (1::Int) :. All)) -- take the second row
-- computeUnboxedS (R.extract (Z :. 1 :. 1) (Z :. 3 :. 3) a)

c = fromListUnboxed (Z :. 3 :. 3 :. 3) [0..26] :: Array U DIM3 Int

--lnRow n = computeUnboxedS $ slice a (R.Any :. n :. R.All)

fun3D_k :: (Source r b, Shape (((Z :. head1) :. head2) :. head3), Num head3)
        => Array r (Z :. head1 :. head2 :. head3) b -> Array D (Z :. head1 :. head2) b
fun3D_k arr = R.traverse arr (\(Z :. i :. j :. _ ) -> (Z:. i :. j)) (\f (Z :. i :. j )  -> f (Z :. i :. j :. 0))
