import Data.Monoid
-- Question from Guy Steele

-- maxgen is just a poor mans scanl!
maxGen :: Ord t1 => (t -> t1) -> [t] -> [(t, t1)]
maxGen _   []     = []
maxGen key (x:xs) = foldl func [(x, key x)] xs
  where
    func ys@((val,greatest):_) x = (x, max greatest (key val)) : ys

maxLeft :: Ord t1 => (t -> t1) -> [t] -> [(t, t1)]
maxLeft = (reverse .) . maxGen

maxRight :: Ord t1 => (t -> t1) -> [t] -> [(t, t1)]
maxRight key = maxGen key . reverse

waterSustained :: (Ord a, Num a) => [a] -> a
waterSustained = foldl func 0 . maxR . maxL
  where
    maxL = maxLeft id   -- could just use scanl instead!
    maxR = maxRight fst -- would make 3 different lists though!
    func acc ((val,lMax), rMax) = acc + max (smaller - val) 0
      where smaller = min rMax lMax


-- ask by someone I know
-- Imagine you have rectangles on a chart, and some of them overlap, what is their skyline?
-- Where their skyline is the max of the two coordinates

data Rectangle = Rec {x1 :: Int
                     ,x2 :: Int
                     ,y  :: Int
                     } deriving Show

given :: [Rectangle]
given = [Rec 3  9 10
        ,Rec 4  7 15
        ,Rec 5  12 12
        ,Rec 15 20 10
        ,Rec 19 24 9
        ]

zipWithDefaultF :: (a1 -> a2 -> a3) -> a1 -> a2 -> [a1] -> [a2] -> [a3]
zipWithDefaultF f dx dy = iter
  where
    iter (x:xs) (y:ys) = f x y  : iter xs ys
    iter (x:xs) []     = f x dy : iter xs []
    iter []     (y:ys) = f dx y : iter [] ys
    iter []     []     = []

zipWithSameF :: (a2 -> a2 -> a3) -> a2 -> [a2] -> [a2] -> [a3]
zipWithSameF f dx = zipWithDefaultF f dx dx


rectToList :: Rectangle -> [Int]
rectToList (Rec x1 x2 y) = replicate x1 0 <> replicate diff y
  where
    diff = x2 - x1 + 1

skyLine :: (Foldable f, Functor f) => f Rectangle -> [Int]
skyLine = foldr f [] . fmap rectToList
  where
    f = zipWithSameF max 0

--skyLine given
--[0,0,0,10,15,15,15,15,12,12,12,12,12,0,0,10,10,10,10,10,10,9,9,9,9]
