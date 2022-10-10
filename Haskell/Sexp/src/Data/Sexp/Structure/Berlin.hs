module Data.Sexp.Structure.Berlin where

import Mari.Library
import qualified Mari.Library.NameSymbol as NameSymbol
import qualified Data.Sexp as Sexp
import Data.Sexp.Structure.Helpers

newtype Relocated = Relocated
  { relocatedName :: NameSymbol.T
  }
  deriving (Show)

----------------------------------------
-- Relocated
----------------------------------------

nameRelocated :: NameSymbol.T
nameRelocated = ":relocated"

isRelocated :: Sexp.T -> Bool
isRelocated (Sexp.Cons form _) = Sexp.isAtomNamed form nameRelocated
isRelocated _ = False

toRelocated :: Sexp.T -> Maybe Relocated
toRelocated form
  | isRelocated form =
    case form of
      _nameRelocated Sexp.:> nameSymbol1 Sexp.:> Sexp.Nil
        | Just nameSymbol1 <- toNameSymbol nameSymbol1 ->
          Relocated nameSymbol1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromRelocated :: Relocated -> Sexp.T
fromRelocated (Relocated nameSymbol1) =
  Sexp.list [Sexp.atom nameRelocated, fromNameSymbol nameSymbol1]

instance Sexp.Serialize Relocated where
  deserialize = toRelocated
  serialize = fromRelocated
