import Data.Semigroup
import Control.Applicative
import Data.Char (isSpace)
import Numeric.Natural
import Data.Bits(xor)
import Data.List(mapAccumR, mapAccumL)
-- Question from Guy Steele

-- maxgen is just a poor mans scanl!
maxGen :: Ord t1 => (t -> t1) -> [t] -> [(t, t1)]
maxGen _   []     = []
maxGen key (x:xs) = foldl func [(x, key x)] xs
  where
    func ys@((val,greatest):_) x = (x, max greatest (key val)) : ys
    func [] _ = error "should never happen"

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

-- beginner number to string problem----------------------------------------------------------------
trimEnd :: String -> String
trimEnd = reverse . dropWhile isSpace . reverse

concatExtension :: Semigroup a => a -> a -> a -> a
concatExtension s x y = x <> s <> y

convertSingleNoZero :: Char -> Maybe String
convertSingleNoZero '0' = Just ""
convertSingleNoZero x   = convertSingle x

convertSingle :: Char -> Maybe String
convertSingle '0' = Just "zero"
convertSingle '1' = Just "one"
convertSingle '2' = Just "two"
convertSingle '3' = Just "three"
convertSingle '4' = Just "four"
convertSingle '5' = Just "five"
convertSingle '6' = Just "six"
convertSingle '7' = Just "seven"
convertSingle '8' = Just "eight"
convertSingle '9' = Just "nine"
convertSingle _   = Nothing

convertTensNoZero :: String -> Maybe String
convertTensNoZero ['0',y] = convertSingleNoZero y
convertTensNoZero xs      = convertTens xs

convertTens :: String -> Maybe String
convertTens ['0',y] = convertSingle y
convertTens "10"    = Just "ten"
convertTens "11"    = Just "eleven"
convertTens "12"    = Just "twelve"
convertTens "13"    = Just "thirteen"
convertTens "15"    = Just "fifteen"
convertTens ['1',y] = (<> "ty") <$> convertSingle y
convertTens ['2',y] = (trimEnd . ("twenty " <>)) <$> convertSingleNoZero y
convertTens ['3',y] = (trimEnd . ("thirty " <>)) <$> convertSingleNoZero y
convertTens ['5',y] = (trimEnd . ("fifty "  <>)) <$> convertSingleNoZero y
convertTens [x,'0'] = (<> "ty") <$> convertSingle x
convertTens [x,y]   = concatExtension "ty " <$> convertSingle x <*> convertSingle y
convertTens _       = Nothing

convertHundreds :: String -> Maybe String
convertHundreds ('0':xs) = convertTens xs
convertHundreds (x:xs)   = concatExtension " hundred " <$> convertSingle x <*> convertTensNoZero xs
convertHundreds []       = Nothing

thousandExtension = concatExtension " thousand "

convertThousands 6 xs     = thousandExtension <$> convertHundreds (take 3 xs) <*> convertHundreds (drop 3 xs)
convertThousands 5 xs     = thousandExtension <$> convertTens     (take 2 xs) <*> convertHundreds (drop 2 xs)
convertThousands 4 (x:xs) = thousandExtension <$> convertSingle x <*> convertHundreds xs
convertThousands l xs     = convertHundreds xs

convert :: (Num p, Show p) => p -> Maybe [Char]
convert num
  | len >= 4  = convertThousands len showN
  | len == 3  = convertHundreds showN
  | len == 2  = convertTens showN
  | len == 1  = convertSingle (head showN)
  | otherwise = Nothing
  where
    showN = show num
    len   = length showN


-- Multiplying lists of numbers ------------------------------------------------------------

data Numb
  = N { negative :: Bool
      , numbers :: [Natural]
      }
  deriving (Show, Eq)

mult :: Numb -> Numb -> Numb
mult (N neg1 numb1) (N neg2 numb2) =
  N (xor neg1 neg2) (dropZeros (foldr addSingleNats [] (extraZeros numbersMulted)))
  where
    dropZeros     = dropWhile (== 0)
    numbersMulted = scalorMult <$> numb2
    scalorMult x  = (x *) <$> numb1

addSingleNats :: [Natural] -> [Natural] -> [Natural]
addSingleNats xs ys =
  carry : arr
  where
    (carry,arr)   = mapAccumR f 0 added
    (nXs,nYs)     = padFront xs ys
    added         = zipWith (+) nXs nYs
    f i carryOver = ((i + carryOver) `div` 10, (i + carryOver) `rem` 10)


padFront :: (Num a1, Num a2) => [a2] -> [a1] -> ([a2], [a1])
padFront xs ys
  | xsl > ysl =
    (xs, replicate (xsl - ysl) 0 <> ys)
  | xsl < ysl =
    (replicate (ysl - xsl) 0 <> xs, ys)
  | otherwise = (xs,ys)
  where
    xsl = length xs
    ysl = length ys


extraZeros :: Num a => [[a]] -> [[a]]
extraZeros xss = recur xss len
  where
    len = length xss - 1
    recur (xs:xss) i = xs <> replicate i 0 : recur xss (pred i)
    recur [] _ = []


-- sort like x1 ≤ x2 ≥ x3 ≤ x4 ≥ x5 ...
sortOdd :: Ord a => [a] -> [a]
sortOdd = sortOdd' . sortFirst

sortOdd' []       = []
sortOdd' (x : xs) = ordered <> [last]
  where
    ((last, _),ordered) = mapAccumL f (x, True) xs
    f (ele, b) n
      | (ele < n && b) || ele > n =
        ((n, not b), ele)
      | otherwise =
        ((ele, not b), n)

sortFirst [x] = [x]
sortFirst []  = []
sortFirst (x : x' : xs)
  | x < x'    = x  : x' : xs
  | otherwise = x' : x  : xs
