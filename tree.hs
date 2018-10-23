{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

import Data.Traversable
import Data.Foldable

data Tree a = Node a
            | Empty
            | Branch (Tree a) a (Tree a)
            deriving (Functor, Traversable, Foldable, Show)

test :: Tree Int
test = Branch (Node 2) 5 (Node 13)

addAllGreater :: (Num c, Traversable t) => t c -> (c, t c)
addAllGreater = mapAccumR (\x acc -> (x + acc, x + acc)) 0

minDiff  :: (Read a, Bounded a, Num a, Ord a) => Tree a -> a
minDiff = snd . foldTreeL (\(lst,min') x -> (x, min (abs (x - lst)) min')) (0, maxBound)

-- toList $ T.mapAccumR (\x xs -> (x + xs, x + xs)) 0 (Node 5 [Node 2 [], Node 13 []])


foldTreeR :: (a -> p -> p) -> p -> Tree a -> p
foldTreeR f acc Empty                                  = acc
foldTreeR f acc (Node a)                               = f a acc
foldTreeR f acc (Branch a@(Branch {}) b c@(Branch {})) = foldTreeR f (foldTreeR f (f b acc) c) a
foldTreeR f acc (Branch a@(Branch {}) b c)             = foldTreeR f (f b (newF acc f (foldHelp c))) a
foldTreeR f acc (Branch a b c@(Branch {}))             = (newF (f b (foldTreeR f acc c)) f (foldHelp a))
foldTreeR f acc (Branch a b c)                         = newF (f b (newF acc f (foldHelp c))) f (foldHelp a)

foldTreeL f acc Empty                                  = acc
foldTreeL f acc (Node a)                               = f acc a
foldTreeL f acc (Branch a@(Branch {}) b c@(Branch {})) = foldTreeL f (f (foldTreeL f acc a) b) c
foldTreeL f acc (Branch a@(Branch {}) b c)             = (newFL (f (foldTreeL f acc a) b) f (foldHelp c))
foldTreeL f acc (Branch a b c@(Branch {}))             = foldTreeL f (f (newFL acc f (foldHelp a)) b) c
foldTreeL f acc (Branch a b c)                         = newFL (f (newFL acc f (foldHelp a)) b) f (foldHelp c)

newFL acc f (Just x) = f acc x
newFL acc f Nothing  = acc

newF acc f (Just x) = f x acc
newF acc f Nothing  = acc

foldHelp (Node a) = Just a
foldHelp Empty    = Nothing
