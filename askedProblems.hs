import           Control.Monad.Writer.Lazy as W
import qualified Data.Set                  as S
import           Data.Monoid

-- asked by someone
-- is it possible to remove 0 or more characters from a string S1 = string S2

isSameRem :: String -> String -> Bool
isSameRem (x:xs) (y:ys)
  | x == y    = isSameRem xs ys
  | otherwise = isSameRem xs (y:ys)
isSameRem _  [] = True
isSameRem [] _  = False

isSameRem' :: String -> String -> Bool
isSameRem' xs ys = null $ foldl f ys xs
  where f []     x             = []
        f (y:ys) x | x == y    = ys
                   | otherwise = y:ys


-- if the character is not valid write it to a log
-- we are doing this over lines of strings
correctBenignErrorsL :: (Traversable t, MonadWriter [Char] f) => t Char -> f (t Char)
correctBenignErrorsL xs = traverse f xs
  where
    f x | S.member x validChar = return x
        | otherwise            = tell ("Benign value " <> [x] <> " was changed to space \n")
                              >> return '_'
    validChar = S.fromList "|_"

correctBenignErrors :: Monad m => [String] -> m ([String], String)
correctBenignErrors = W.runWriterT . traverse correctBenignErrorsL
