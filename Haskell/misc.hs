{-# LANGUAGE FlexibleContexts #-}

import Data.Foldable
import Data.List
import qualified Data.Set as S
import Control.Monad.State.Strict
import Control.Monad.Memo

countCombos n xs = length $ execState (f n []) S.empty
  where
    f n acc
      | n == 0    = modify (S.insert (sort acc))
      | n < 0     = return ()
      | otherwise = traverse_ (\x -> f (n - x) (x : acc)) xs

countCombos' n = startEvalMemo . f n
  where
    f n [] = return 0
    f n (x:xs)
      | n == 0    = return 1
      | n < 0     = return 0
      | otherwise = (+) <$> for2 memo f (n - x) (x:xs) <*> for2 memo f n xs
