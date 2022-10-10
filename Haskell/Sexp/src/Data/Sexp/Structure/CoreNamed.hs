-- |
-- Hardening S-expressions into a more readable form. Here we use a
-- mixture of record structures and aliases. Each cover a form that we
-- wish to talk about rather than just match away at
--
-- - _The form for transformation follows this structure_
-- #+begin_src haskell
--   -- the data type
--   data Form = ... deriving (Show)
--   is<Form>   :: Sexp.T -> Bool
--   to<Form>   :: Sexp.T -> Maybe <Form>
--   from<Form> :: <Form> -> Sexp.T
-- #+end_src
-- + With the following properties of the forms
--   #+begin_src haskell
--     ∀ s : Sexp.T. is<Form> s = True ⟷ is-just (to<Form> s)
--
--     to<Form> 。 from<Form> = Just
--   #+end_src
-- _TODO_
--  1. Figure out if we can even express a spec system in
--     Haskell... =to<Form>= and =From<From>= have the exact same signature
--  2. replace the repeat code with the =to<Form>= with an abstraction
--  3. put the meta data with the form so we don't have to do it by
--     hand in the code that uses this
--     1. Use =Mari.Library.LineNum=
--     2. append the =Form= With this
--     3. have to<Form> fill this
--     4. Have extra smart consturctors that are =<form>=, so that we
--        can automatically fill in this meta data
module Data.Sexp.Structure.CoreNamed where

import Mari.Library hiding (Field, Meta, fromInteger, toInteger)
import qualified Mari.Library.NameSymbol as NameSymbol
import qualified Data.Sexp as Sexp
import Data.Sexp.Structure.Helpers

newtype Star = Star {starUniverse :: Integer} deriving (Show)

newtype PrimTy = PrimTy {primTyPrim :: Sexp.T} deriving (Show)

newtype Prim = Prim {primPrim :: Sexp.T} deriving (Show)

data Pi = Pi
  { piBinder :: Binder,
    piBody :: Sexp.T
  }
  deriving (Show)

data Binder = Binder
  { binderName :: NameSymbol.T,
    binderUsage :: Sexp.T,
    binderTerm :: Sexp.T
  }
  deriving (Show)

data Lam = Lam
  { lamName :: NameSymbol.T,
    lamBody :: Sexp.T
  }
  deriving (Show)

data Sigma = Sigma
  { sigmaBinder :: Binder,
    sigmaUsage :: Sexp.T
  }
  deriving (Show)

data Pair = Pair
  { pairFst :: Sexp.T,
    pairSnd :: Sexp.T
  }
  deriving (Show)

data Let = Let
  { letBinder :: Binder,
    letBody :: Sexp.T
  }
  deriving (Show)

data CatProduct = CatProduct
  { catProductFst :: Sexp.T,
    catProductSnd :: Sexp.T
  }
  deriving (Show)

data CatProductIntro = CatProductIntro
  { catProductIntroFst :: Sexp.T,
    catProductIntroSnd :: Sexp.T
  }
  deriving (Show)

data CatProductElimLeft = CatProductElimLeft
  { catCoProductElimLeftType :: Sexp.T,
    catCoProductElimLeftTerm :: Sexp.T
  }
  deriving (Show)

data CatProductElimRight = CatProductElimRight
  { catCoProductElimRightType :: Sexp.T,
    catCoProductElimRightTerm :: Sexp.T
  }
  deriving (Show)

data CatCoProduct = CatCoProduct
  { catCoProductLeft :: Sexp.T,
    catCoProductRight :: Sexp.T
  }
  deriving (Show)

data CatCoproductIntroLeft = CatCoproductIntroLeft
  { catCoproductIntroLeftTerm :: Sexp.T
  }
  deriving (Show)

data CatCoproductIntroRight = CatCoproductIntroRight
  { catCoproductIntroRightTerm :: Sexp.T
  }
  deriving (Show)

data CatCoproductElim = CatCoproductElim
  { catCoproductTerm :: Sexp.T,
    catCoproductElimLeftType :: Sexp.T,
    catCoproductElimLeft :: Sexp.T,
    catCoproductElimRightType :: Sexp.T,
    catCoproductElimRight :: Sexp.T
  }
  deriving (Show)

-- Elim

newtype Var = Var {varName :: NameSymbol.T} deriving (Show)

data App = App
  { appFun :: Sexp.T,
    appArg :: Sexp.T
  }
  deriving (Show)

data Ann = Ann
  { annTerm :: Sexp.T,
    annTy :: Sexp.T
  }
  deriving (Show)

data Meta = Meta
  { metaUsage :: Sexp.T,
    metaUniverse :: Integer
  }
  deriving (Show)

-- Pattern

newtype Dot = Dot {dotBody :: Sexp.T} deriving (Show)

data RawFunClause = RawFunClause
  { rawFunClauseTele :: Sexp.T,
    rawFunClausePats :: Sexp.T,
    rawFunClauseBody :: Sexp.T,
    rawFunClauseCatchAll :: Sexp.T
  }
  deriving (Show)

data Field = Field
  { fieldName :: NameSymbol.T,
    fieldUsage :: Sexp.T,
    fieldTy :: Sexp.T
  }
  deriving (Show)

newtype RecordTy = RecordTy
  { recordTyFields :: [Field]
  }
  deriving (Show)

data Lookup = Lookup
  { lookupBase :: Sexp.T,
    lookupNames :: [Symbol]
  }
  deriving (Show)

--------------------------------------------------------------------------------
-- Automatically Generated code
--------------------------------------------------------------------------------
----------------------------------------
-- Star
----------------------------------------

nameStar :: NameSymbol.T
nameStar = ":star"

isStar :: Sexp.T -> Bool
isStar (Sexp.Cons form _) = Sexp.isAtomNamed form nameStar
isStar _ = False

toStar :: Sexp.T -> Maybe Star
toStar form
  | isStar form =
    case form of
      _nameStar Sexp.:> integer1 Sexp.:> Sexp.Nil
        | Just integer1 <- toInteger integer1 ->
          Star integer1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromStar :: Star -> Sexp.T
fromStar (Star integer1) =
  Sexp.list [Sexp.atom nameStar, fromInteger integer1]

instance Sexp.Serialize Star where
  deserialize = toStar
  serialize = fromStar

----------------------------------------
-- PrimTy
----------------------------------------

namePrimTy :: NameSymbol.T
namePrimTy = ":prim-ty"

isPrimTy :: Sexp.T -> Bool
isPrimTy (Sexp.Cons form _) = Sexp.isAtomNamed form namePrimTy
isPrimTy _ = False

toPrimTy :: Sexp.T -> Maybe PrimTy
toPrimTy form
  | isPrimTy form =
    case form of
      _namePrimTy Sexp.:> sexp1 Sexp.:> Sexp.Nil ->
        PrimTy sexp1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromPrimTy :: PrimTy -> Sexp.T
fromPrimTy (PrimTy sexp1) =
  Sexp.list [Sexp.atom namePrimTy, sexp1]

instance Sexp.Serialize PrimTy where
  deserialize = toPrimTy
  serialize = fromPrimTy

----------------------------------------
-- Prim
----------------------------------------

namePrim :: NameSymbol.T
namePrim = ":prim"

isPrim :: Sexp.T -> Bool
isPrim (Sexp.Cons form _) = Sexp.isAtomNamed form namePrim
isPrim _ = False

toPrim :: Sexp.T -> Maybe Prim
toPrim form
  | isPrim form =
    case form of
      _namePrim Sexp.:> sexp1 Sexp.:> Sexp.Nil ->
        Prim sexp1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromPrim :: Prim -> Sexp.T
fromPrim (Prim sexp1) =
  Sexp.list [Sexp.atom namePrim, sexp1]

instance Sexp.Serialize Prim where
  deserialize = toPrim
  serialize = fromPrim

----------------------------------------
-- Pi
----------------------------------------

namePi :: NameSymbol.T
namePi = ":pi"

isPi :: Sexp.T -> Bool
isPi (Sexp.Cons form _) = Sexp.isAtomNamed form namePi
isPi _ = False

toPi :: Sexp.T -> Maybe Pi
toPi form
  | isPi form =
    case form of
      _namePi Sexp.:> binder1 Sexp.:> sexp2 Sexp.:> Sexp.Nil
        | Just binder1 <- toBinder binder1 ->
          Pi binder1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromPi :: Pi -> Sexp.T
fromPi (Pi binder1 sexp2) =
  Sexp.list [Sexp.atom namePi, fromBinder binder1, sexp2]

instance Sexp.Serialize Pi where
  deserialize = toPi
  serialize = fromPi

----------------------------------------
-- Binder
----------------------------------------

toBinder :: Sexp.T -> Maybe Binder
toBinder form =
  case form of
    nameSymbol1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> Sexp.Nil
      | Just nameSymbol1 <- toNameSymbol nameSymbol1 ->
        Binder nameSymbol1 sexp2 sexp3 |> Just
    _ ->
      Nothing

fromBinder :: Binder -> Sexp.T
fromBinder (Binder nameSymbol1 sexp2 sexp3) =
  Sexp.list [fromNameSymbol nameSymbol1, sexp2, sexp3]

instance Sexp.Serialize Binder where
  deserialize = toBinder
  serialize = fromBinder

----------------------------------------
-- Lam
----------------------------------------

nameLam :: NameSymbol.T
nameLam = ":named-lambda"

isLam :: Sexp.T -> Bool
isLam (Sexp.Cons form _) = Sexp.isAtomNamed form nameLam
isLam _ = False

toLam :: Sexp.T -> Maybe Lam
toLam form
  | isLam form =
    case form of
      _nameLam Sexp.:> nameSymbol1 Sexp.:> sexp2 Sexp.:> Sexp.Nil
        | Just nameSymbol1 <- toNameSymbol nameSymbol1 ->
          Lam nameSymbol1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLam :: Lam -> Sexp.T
fromLam (Lam nameSymbol1 sexp2) =
  Sexp.list [Sexp.atom nameLam, fromNameSymbol nameSymbol1, sexp2]

instance Sexp.Serialize Lam where
  deserialize = toLam
  serialize = fromLam

----------------------------------------
-- Sigma
----------------------------------------

nameSigma :: NameSymbol.T
nameSigma = ":sigma"

isSigma :: Sexp.T -> Bool
isSigma (Sexp.Cons form _) = Sexp.isAtomNamed form nameSigma
isSigma _ = False

toSigma :: Sexp.T -> Maybe Sigma
toSigma form
  | isSigma form =
    case form of
      _nameSigma Sexp.:> binder1 Sexp.:> sexp2 Sexp.:> Sexp.Nil
        | Just binder1 <- toBinder binder1 ->
          Sigma binder1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromSigma :: Sigma -> Sexp.T
fromSigma (Sigma binder1 sexp2) =
  Sexp.list [Sexp.atom nameSigma, fromBinder binder1, sexp2]

instance Sexp.Serialize Sigma where
  deserialize = toSigma
  serialize = fromSigma

----------------------------------------
-- Pair
----------------------------------------

namePair :: NameSymbol.T
namePair = ":pair"

isPair :: Sexp.T -> Bool
isPair (Sexp.Cons form _) = Sexp.isAtomNamed form namePair
isPair _ = False

toPair :: Sexp.T -> Maybe Pair
toPair form
  | isPair form =
    case form of
      _namePair Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        Pair sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromPair :: Pair -> Sexp.T
fromPair (Pair sexp1 sexp2) =
  Sexp.list [Sexp.atom namePair, sexp1, sexp2]

instance Sexp.Serialize Pair where
  deserialize = toPair
  serialize = fromPair

----------------------------------------
-- Let
----------------------------------------

nameLet :: NameSymbol.T
nameLet = ":named-let"

isLet :: Sexp.T -> Bool
isLet (Sexp.Cons form _) = Sexp.isAtomNamed form nameLet
isLet _ = False

toLet :: Sexp.T -> Maybe Let
toLet form
  | isLet form =
    case form of
      _nameLet Sexp.:> binder1 Sexp.:> sexp2 Sexp.:> Sexp.Nil
        | Just binder1 <- toBinder binder1 ->
          Let binder1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLet :: Let -> Sexp.T
fromLet (Let binder1 sexp2) =
  Sexp.list [Sexp.atom nameLet, fromBinder binder1, sexp2]

instance Sexp.Serialize Let where
  deserialize = toLet
  serialize = fromLet

----------------------------------------
-- Var
----------------------------------------

toVar :: Sexp.T -> Maybe Var
toVar form =
  case form of
    nameSymbol1 Sexp.:> Sexp.Nil
      | Just nameSymbol1 <- toNameSymbol nameSymbol1 ->
        Var nameSymbol1 |> Just
    _ ->
      Nothing

fromVar :: Var -> Sexp.T
fromVar (Var nameSymbol1) =
  Sexp.list [fromNameSymbol nameSymbol1]

instance Sexp.Serialize Var where
  deserialize = toVar
  serialize = fromVar

----------------------------------------
-- App
----------------------------------------

toApp :: Sexp.T -> Maybe App
toApp form =
  case form of
    sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
      App sexp1 sexp2 |> Just
    _ ->
      Nothing

fromApp :: App -> Sexp.T
fromApp (App sexp1 sexp2) =
  Sexp.list [sexp1, sexp2]

instance Sexp.Serialize App where
  deserialize = toApp
  serialize = fromApp

----------------------------------------
-- Ann
----------------------------------------

nameAnn :: NameSymbol.T
nameAnn = ":"

isAnn :: Sexp.T -> Bool
isAnn (Sexp.Cons form _) = Sexp.isAtomNamed form nameAnn
isAnn _ = False

toAnn :: Sexp.T -> Maybe Ann
toAnn form
  | isAnn form =
    case form of
      _nameAnn Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        Ann sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromAnn :: Ann -> Sexp.T
fromAnn (Ann sexp1 sexp2) =
  Sexp.list [Sexp.atom nameAnn, sexp1, sexp2]

instance Sexp.Serialize Ann where
  deserialize = toAnn
  serialize = fromAnn

----------------------------------------
-- Meta
----------------------------------------

toMeta :: Sexp.T -> Maybe Meta
toMeta form =
  case form of
    sexp1 Sexp.:> integer2 Sexp.:> Sexp.Nil
      | Just integer2 <- toInteger integer2 ->
        Meta sexp1 integer2 |> Just
    _ ->
      Nothing

fromMeta :: Meta -> Sexp.T
fromMeta (Meta sexp1 integer2) =
  Sexp.list [sexp1, fromInteger integer2]

instance Sexp.Serialize Meta where
  deserialize = toMeta
  serialize = fromMeta

----------------------------------------
-- Field
----------------------------------------

toField :: Sexp.T -> Maybe Field
toField form =
  case form of
    nameSymbol1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> Sexp.Nil
      | Just nameSymbol1 <- toNameSymbol nameSymbol1 ->
        Field nameSymbol1 sexp2 sexp3 |> Just
    _ ->
      Nothing

fromField :: Field -> Sexp.T
fromField (Field nameSymbol1 sexp2 sexp3) =
  Sexp.list [fromNameSymbol nameSymbol1, sexp2, sexp3]

instance Sexp.Serialize Field where
  deserialize = toField
  serialize = fromField

----------------------------------------
-- RecordTy
----------------------------------------

nameRecordTy :: NameSymbol.T
nameRecordTy = ":record-ty"

isRecordTy :: Sexp.T -> Bool
isRecordTy (Sexp.Cons form _) = Sexp.isAtomNamed form nameRecordTy
isRecordTy _ = False

toRecordTy :: Sexp.T -> Maybe RecordTy
toRecordTy form
  | isRecordTy form =
    case form of
      _nameRecordTy Sexp.:> field1
        | Just field1 <- toField `fromStarList` field1 ->
          RecordTy field1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromRecordTy :: RecordTy -> Sexp.T
fromRecordTy (RecordTy field1) =
  Sexp.listStar [Sexp.atom nameRecordTy, fromField `toStarList` field1]

instance Sexp.Serialize RecordTy where
  deserialize = toRecordTy
  serialize = fromRecordTy

----------------------------------------
-- Lookup
----------------------------------------

nameLookup :: NameSymbol.T
nameLookup = ":lookup"

isLookup :: Sexp.T -> Bool
isLookup (Sexp.Cons form _) = Sexp.isAtomNamed form nameLookup
isLookup _ = False

toLookup :: Sexp.T -> Maybe Lookup
toLookup form
  | isLookup form =
    case form of
      _nameLookup Sexp.:> sexp1 Sexp.:> symbol2
        | Just symbol2 <- toSymbol `fromStarList` symbol2 ->
          Lookup sexp1 symbol2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLookup :: Lookup -> Sexp.T
fromLookup (Lookup sexp1 symbol2) =
  Sexp.listStar [Sexp.atom nameLookup, sexp1, fromSymbol `toStarList` symbol2]

instance Sexp.Serialize Lookup where
  deserialize = toLookup
  serialize = fromLookup

----------------------------------------
-- CatProduct
----------------------------------------

nameCatProduct :: NameSymbol.T
nameCatProduct = ":cat-product"

isCatProduct :: Sexp.T -> Bool
isCatProduct (Sexp.Cons form _) = Sexp.isAtomNamed form nameCatProduct
isCatProduct _ = False

toCatProduct :: Sexp.T -> Maybe CatProduct
toCatProduct form
  | isCatProduct form =
    case form of
      _nameCatProduct Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        CatProduct sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromCatProduct :: CatProduct -> Sexp.T
fromCatProduct (CatProduct sexp1 sexp2) =
  Sexp.list [Sexp.atom nameCatProduct, sexp1, sexp2]

instance Sexp.Serialize CatProduct where
  deserialize = toCatProduct
  serialize = fromCatProduct

----------------------------------------
-- CatProductIntro
----------------------------------------

nameCatProductIntro :: NameSymbol.T
nameCatProductIntro = ":cat-product-intro"

isCatProductIntro :: Sexp.T -> Bool
isCatProductIntro (Sexp.Cons form _) = Sexp.isAtomNamed form nameCatProductIntro
isCatProductIntro _ = False

toCatProductIntro :: Sexp.T -> Maybe CatProductIntro
toCatProductIntro form
  | isCatProductIntro form =
    case form of
      _nameCatProductIntro Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        CatProductIntro sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromCatProductIntro :: CatProductIntro -> Sexp.T
fromCatProductIntro (CatProductIntro sexp1 sexp2) =
  Sexp.list [Sexp.atom nameCatProductIntro, sexp1, sexp2]

instance Sexp.Serialize CatProductIntro where
  deserialize = toCatProductIntro
  serialize = fromCatProductIntro

----------------------------------------
-- CatProductElimLeft
----------------------------------------

nameCatProductElimLeft :: NameSymbol.T
nameCatProductElimLeft = ":cat-elim-left"

isCatProductElimLeft :: Sexp.T -> Bool
isCatProductElimLeft (Sexp.Cons form _) = Sexp.isAtomNamed form nameCatProductElimLeft
isCatProductElimLeft _ = False

toCatProductElimLeft :: Sexp.T -> Maybe CatProductElimLeft
toCatProductElimLeft form
  | isCatProductElimLeft form =
    case form of
      _nameCatProductElimLeft Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        CatProductElimLeft sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromCatProductElimLeft :: CatProductElimLeft -> Sexp.T
fromCatProductElimLeft (CatProductElimLeft sexp1 sexp2) =
  Sexp.list [Sexp.atom nameCatProductElimLeft, sexp1, sexp2]

instance Sexp.Serialize CatProductElimLeft where
  deserialize = toCatProductElimLeft
  serialize = fromCatProductElimLeft

----------------------------------------
-- CatProductElimRight
----------------------------------------

nameCatProductElimRight :: NameSymbol.T
nameCatProductElimRight = ":cat-elim-right"

isCatProductElimRight :: Sexp.T -> Bool
isCatProductElimRight (Sexp.Cons form _) = Sexp.isAtomNamed form nameCatProductElimRight
isCatProductElimRight _ = False

toCatProductElimRight :: Sexp.T -> Maybe CatProductElimRight
toCatProductElimRight form
  | isCatProductElimRight form =
    case form of
      _nameCatProductElimRight Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        CatProductElimRight sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromCatProductElimRight :: CatProductElimRight -> Sexp.T
fromCatProductElimRight (CatProductElimRight sexp1 sexp2) =
  Sexp.list [Sexp.atom nameCatProductElimRight, sexp1, sexp2]

instance Sexp.Serialize CatProductElimRight where
  deserialize = toCatProductElimRight
  serialize = fromCatProductElimRight

----------------------------------------
-- CatCoProduct
----------------------------------------

nameCatCoProduct :: NameSymbol.T
nameCatCoProduct = ":cat-coproduct"

isCatCoProduct :: Sexp.T -> Bool
isCatCoProduct (Sexp.Cons form _) = Sexp.isAtomNamed form nameCatCoProduct
isCatCoProduct _ = False

toCatCoProduct :: Sexp.T -> Maybe CatCoProduct
toCatCoProduct form
  | isCatCoProduct form =
    case form of
      _nameCatCoProduct Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        CatCoProduct sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromCatCoProduct :: CatCoProduct -> Sexp.T
fromCatCoProduct (CatCoProduct sexp1 sexp2) =
  Sexp.list [Sexp.atom nameCatCoProduct, sexp1, sexp2]

instance Sexp.Serialize CatCoProduct where
  deserialize = toCatCoProduct
  serialize = fromCatCoProduct

----------------------------------------
-- CatCoproductIntroLeft
----------------------------------------

nameCatCoproductIntroLeft :: NameSymbol.T
nameCatCoproductIntroLeft = ":cat-intro-left"

isCatCoproductIntroLeft :: Sexp.T -> Bool
isCatCoproductIntroLeft (Sexp.Cons form _) = Sexp.isAtomNamed form nameCatCoproductIntroLeft
isCatCoproductIntroLeft _ = False

toCatCoproductIntroLeft :: Sexp.T -> Maybe CatCoproductIntroLeft
toCatCoproductIntroLeft form
  | isCatCoproductIntroLeft form =
    case form of
      _nameCatCoproductIntroLeft Sexp.:> sexp1 Sexp.:> Sexp.Nil ->
        CatCoproductIntroLeft sexp1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromCatCoproductIntroLeft :: CatCoproductIntroLeft -> Sexp.T
fromCatCoproductIntroLeft (CatCoproductIntroLeft sexp1) =
  Sexp.list [Sexp.atom nameCatCoproductIntroLeft, sexp1]

instance Sexp.Serialize CatCoproductIntroLeft where
  deserialize = toCatCoproductIntroLeft
  serialize = fromCatCoproductIntroLeft

----------------------------------------
-- CatCoproductIntroRight
----------------------------------------

nameCatCoproductIntroRight :: NameSymbol.T
nameCatCoproductIntroRight = ":cat-intro-right"

isCatCoproductIntroRight :: Sexp.T -> Bool
isCatCoproductIntroRight (Sexp.Cons form _) = Sexp.isAtomNamed form nameCatCoproductIntroRight
isCatCoproductIntroRight _ = False

toCatCoproductIntroRight :: Sexp.T -> Maybe CatCoproductIntroRight
toCatCoproductIntroRight form
  | isCatCoproductIntroRight form =
    case form of
      _nameCatCoproductIntroRight Sexp.:> sexp1 Sexp.:> Sexp.Nil ->
        CatCoproductIntroRight sexp1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromCatCoproductIntroRight :: CatCoproductIntroRight -> Sexp.T
fromCatCoproductIntroRight (CatCoproductIntroRight sexp1) =
  Sexp.list [Sexp.atom nameCatCoproductIntroRight, sexp1]

instance Sexp.Serialize CatCoproductIntroRight where
  deserialize = toCatCoproductIntroRight
  serialize = fromCatCoproductIntroRight

----------------------------------------
-- CatCoproductElim
----------------------------------------

nameCatCoproductElim :: NameSymbol.T
nameCatCoproductElim = ":cat-coelim"

isCatCoproductElim :: Sexp.T -> Bool
isCatCoproductElim (Sexp.Cons form _) = Sexp.isAtomNamed form nameCatCoproductElim
isCatCoproductElim _ = False

toCatCoproductElim :: Sexp.T -> Maybe CatCoproductElim
toCatCoproductElim form
  | isCatCoproductElim form =
    case form of
      _nameCatCoproductElim Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> sexp4 Sexp.:> sexp5 Sexp.:> Sexp.Nil ->
        CatCoproductElim sexp1 sexp2 sexp3 sexp4 sexp5 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromCatCoproductElim :: CatCoproductElim -> Sexp.T
fromCatCoproductElim (CatCoproductElim sexp1 sexp2 sexp3 sexp4 sexp5) =
  Sexp.list [Sexp.atom nameCatCoproductElim, sexp1, sexp2, sexp3, sexp4, sexp5]

instance Sexp.Serialize CatCoproductElim where
  deserialize = toCatCoproductElim
  serialize = fromCatCoproductElim
