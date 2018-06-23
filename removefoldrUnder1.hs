#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import System.Directory
import Control.Monad

main :: IO [()]
main = listDirectory "." >>= filterM  doesDirectoryExist
                         >>= filterM  isDirectorySmall
                         >>= traverse removeDirectoryRecursive
  where isDirectorySmall d = (<= 1) . length <$> listDirectory d


main2 :: IO [()]
main2 = listDirectory
        >=> filterM  doesDirectoryExist
        >=> filterM  isDirectorySmall
        >=> traverse removeDirectoryRecursive $ "."
  where isDirectorySmall = ((<= 1) . length <$>) . listDirectory
