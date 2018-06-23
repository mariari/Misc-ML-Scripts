#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text          as T
import           Data.Monoid
import           Turtle             hiding (FilePath, fold)

range :: String
range = ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9']

charsStr :: Char -> Char -> String
charsStr x y = [x,y]

links :: [String]
links = ("https://lainfile.pw/" <>) <$> [[x, y] | x <- range, y <- range]

-- alternate definition of links
links2 :: [String]
links2 = ("https://lainfile.pw/" <>) <$> (charsStr <$> range <*> range)

-- Downloads the requested file to the users path
dwnFile :: MonadIO io => Text -> io ExitCode
dwnFile url = shell ("wget --content-disposition " <> url) empty

main :: IO ()
main = sequence_ (dwnFile . T.pack <$> links)
