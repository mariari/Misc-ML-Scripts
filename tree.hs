{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

import Data.Maybe
import Data.Traversable
import Data.Foldable
import Control.Monad
import Data.List

data Tree a = Node a
            | Empty
            | Branch (Tree a) a (Tree a)
            deriving (Functor, Traversable, Foldable, Show)

test :: Tree Int
test = Branch (Node 2) 5 (Node 13)

test2 :: Tree Int
test2 = ( Branch (Branch (Node 49) 69 (Branch (Node 78) 89 (Node 1000))) 90 Empty )

addAllGreater :: (Num c, Traversable t) => t c -> (c, t c)
addAllGreater = mapAccumR (\x acc -> (x + acc, x + acc)) 0

minDiff  :: (Read a, Bounded a, Num a, Ord a) => Tree a -> a
minDiff = snd . foldTreeL (\(lst,min') x -> (x, min (abs (x - lst)) min')) (0, maxBound)

foldTreeR :: (a -> p -> p) -> p -> Tree a -> p
foldTreeR f acc Empty                                  = acc
foldTreeR f acc (Node a)                               = f a acc
foldTreeR f acc (Branch a@(Branch {}) b c@(Branch {})) = foldTreeR f (foldTreeR f (f b acc) c) a
foldTreeR f acc (Branch a@(Branch {}) b c)             = foldTreeR f (f b (newF acc f (foldHelp c))) a
foldTreeR f acc (Branch a b c@(Branch {}))             = (newF (f b (foldTreeR f acc c)) f (foldHelp a))
foldTreeR f acc (Branch a b c)                         = newF (f b (newF acc f (foldHelp c))) f (foldHelp a)

-- more readable, but the cases are much less obvious in this style!
foldTreeL f acc Empty    = acc
foldTreeL f acc (Node a) = f acc a
foldTreeL f acc (Branch a@(Branch {}) b c@(Branch {})) = foldTreeL f second c
  where first  = foldTreeL f acc a
        second = f first b
foldTreeL f acc (Branch a@(Branch {}) b c) = newFL second f (foldHelp c)
  where first  = foldTreeL f acc a
        second = f first b
foldTreeL f acc (Branch a b c@(Branch {})) = foldTreeL f second c
  where first  = newFL acc f (foldHelp a)
        second = f first b
foldTreeL f acc (Branch a b c) = newFL second f (foldHelp c)
  where first  = newFL acc f (foldHelp a)
        second = f first b

newFL acc f (Just x) = f acc x
newFL acc f Nothing  = acc

newF acc f (Just x) = f x acc
newF acc f Nothing  = acc

foldHelp (Node a) = Just a
foldHelp Empty    = Nothing
foldHelp _        = error "branch should not call this"

isBalanced :: Tree a -> Bool
isBalanced = isJust . depth 0
  where
    depth i (Branch a _ c) = do
      dL <- depth (i + 1) a
      dR <- depth (i + 1) c
      guard ((abs (dL - dR)) <= 1)
      Just (max dR dL)
    depth i (Node _) = Just i
    depth i Empty    = Just i


splitWith :: (a -> Bool) -> [a] -> ([a], [a])
splitWith p xs = recurse [] xs
  where
    recurse acc [] = (reverse acc, [])
    recurse acc (x:xs)
      | p x       = (reverse acc, xs)
      | otherwise = recurse (x:acc) xs

-- the first list is the in order list, the 2nd is the pre order list
preInOrder :: Eq a => [a] -> [a] -> Tree a
preInOrder is []     = Empty -- the lists should be the same size
preInOrder is [p]    = Node p
preInOrder is (p:ps) = Branch (preInOrder l (take sizeL ps)) p (preInOrder r (drop sizeL ps))
   where
     (l,r) = splitWith (== p) is
     sizeL = length l -- we can view this as how many elements have gone to the left partition

-- only works for an ordered tree (often called BST)
preOrder :: Ord a => [a] -> Tree a
preOrder xs = fst (recurse xs)
  where
    recurse [x]      = (Node x, [])
    recurse []       = (Empty, [])
    recurse (x:y:xs)
      | y < x     = (Branch nodeL x nodeR, remaining')
      | otherwise = (Node x, (y:xs))
      where
        (nodeL, remaning)   = recurse (y:xs)
        (nodeR, remaining') = recurse remaning
