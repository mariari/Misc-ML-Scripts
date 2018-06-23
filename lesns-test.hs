import Control.Lens as L hiding ((|>), (<|), index) -- used for easier getting and setting for vectors
import Data.Vector  as V hiding (null,foldr,foldl)
import Data.Monoid 


data Point a = P a a
  deriving (Show,Eq)


type Puzz a = Vector (Vector a)



updateVec :: Vector (Vector a) -> Point Int -> a -> Vector (Vector a)
updateVec vec (P x y) val = vec & (ix x . ix y) .~ val


locMov :: Puzz Int -> Point Int -> Maybe Int
locMov vec (P x y) = vec ^? (ix x . ix y)


-- need a way to combine them, so instead we just do
x :: (Num a, Monoid a) => a
x = view both (1,2)

x''' :: Integer
x''' = getSum $ view both (Sum 1,Sum 2)

x' :: [Integer]
x' = toListOf both (1,2)

x'' :: Integer
x'' = sumOf both (1,2) -- product of for product
