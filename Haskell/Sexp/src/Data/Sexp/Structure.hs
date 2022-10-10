{-# LANGUAGE UndecidableInstances #-}

module Data.Sexp.Structure
  ( to,
    from,
  )
where

import Mari.Library (Maybe)
import qualified Data.Sexp as Sexp

to :: Sexp.Serialize a => Sexp.T -> Maybe a
to = Sexp.deserialize

from :: Sexp.Serialize a => a -> Sexp.T
from = Sexp.serialize
