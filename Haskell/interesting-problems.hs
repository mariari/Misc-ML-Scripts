import Data.Semigroup
import Control.Applicative
import Data.Char (isSpace)
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
