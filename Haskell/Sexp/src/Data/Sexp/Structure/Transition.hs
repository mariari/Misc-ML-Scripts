{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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
module Data.Sexp.Structure.Transition where

import Mari.Library hiding (Handler, Type)
import qualified Mari.Library.NameSymbol as NameSymbol
import qualified Data.Sexp as Sexp
import Data.Sexp.Structure.Helpers
import Data.Sexp.Structure.Parsing (fromLetOp, fromLetRet, fromNotPunnedGroup, toLetOp, toLetRet, toNotPunnedGroup)
import qualified Data.Sexp.Structure.Parsing as Parsing

newtype RecordNoPunned = RecordNoPunned
  { recordNoPunnedValue :: [Parsing.NotPunned]
  }
  deriving (Show)

-- | @Defun-match@ is a matching defun structure
data DefunMatch = DefunMatch
  { defunMatchName :: Sexp.T,
    defunMatchArgs :: [ArgBody]
  }
  deriving (Show)

-- | @ArgBody@ abstracts over the details of arguments and body
data ArgBody = ArgBody
  { argBodyArgs :: Sexp.T,
    argBodyBody :: Sexp.T
  }
  deriving (Show)

newtype LambdaCase = LambdaCase [ArgBody]
  deriving (Show)

-- | @If@ has an pred, then, and else.
data If = If
  { ifPredicate :: Sexp.T,
    ifConclusion :: Sexp.T,
    ifAlternative :: Sexp.T
  }
  deriving (Show)

-- | @IfNoElse@ has a pred and a then.
data IfNoElse = IfNoElse
  { ifNoElsePredicate :: Sexp.T,
    ifNoElseConclusion :: Sexp.T
  }
  deriving (Show)

-- | @ifFull@ is the full range of If with maybe having an else
data IfFull
  = Else If
  | NoElse IfNoElse
  deriving (Show)

data DefunSigMatch = DefunSigMatch
  { defunSigMatchName :: Sexp.T,
    defunSigMatchSig :: Sexp.T,
    defunSigMatchArgs :: [ArgBody]
  }
  deriving (Show)

-- TODO ∷ have a property sexp form!?

data LetMatch = LetMatch
  { letMatchName :: Sexp.T,
    -- args are ungrouped, so we have to handle that in the ouptut
    -- structure
    letMatchArgs :: [ArgBody],
    letMatchBody :: Sexp.T
  }
  deriving (Show)

-- DEPRECATED :: Unsafe to form on
data LetHandler = LetHandler
  { letHandlerName :: Sexp.T,
    letHandlerRet :: Sexp.T,
    letHandlerOps :: Sexp.T
  }
  deriving (Show)

data Handler = Handler
  { handlerName :: Sexp.T,
    handlerRet :: Parsing.LetRet,
    handlerOps :: [Parsing.LetOp]
  }
  deriving (Show)

data SumCon = SumCon
  { sumConTypeOf :: NameSymbol.T
  }
  deriving (Show)

data SumConFilled = SumConFilled
  { sumConFilledTypeOf :: NameSymbol.T,
    sumConFilledDef :: Sexp.T
  }
  deriving (Show)

newtype InPackage = InPackage
  {name :: NameSymbol.T}
  deriving (Show)

--------------------------------------------------------------------------------
-- Converter functions
-- The format for these are
-- name<Form> :: NameSymbol.T
-- is<Form>   :: Sexp.T -> Bool
-- to<Form>   :: Sexp.T -> Maybe <Form>
-- from<Form> :: <Form> -> Sexp.T
--------------------------------------------------------------------------------

-------------------------------------------
-- Not Generated, due to limited generation
-------------------------------------------

toIfFull :: Sexp.T -> Maybe IfFull
toIfFull sexp =
  fmap Else (toIf sexp) <|> fmap NoElse (toIfNoElse sexp)

--------------------
-- Args Bodys
--------------------

-- these are ungrouned fromArgBodys, where we groupBy2 both ways
fromArgBodys :: [ArgBody] -> Sexp.T
fromArgBodys = Sexp.unGroupBy2 . toStarList fromArgBody

-- these are ungrouned fromArgBodys, where we groupBy2 both ways
toArgBodys :: Sexp.T -> Maybe [ArgBody]
toArgBodys = fromStarList toArgBody . Sexp.groupBy2

matchConstructor :: Sexp.T -> Sexp.T
matchConstructor x = Sexp.list [x]

----------------------------------------
-- Generated
----------------------------------------

----------------------------------------
-- ArgBody
----------------------------------------

toArgBody :: Sexp.T -> Maybe ArgBody
toArgBody form =
  case form of
    sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
      ArgBody sexp1 sexp2 |> Just
    _ ->
      Nothing

fromArgBody :: ArgBody -> Sexp.T
fromArgBody (ArgBody sexp1 sexp2) =
  Sexp.list [sexp1, sexp2]

instance Sexp.Serialize ArgBody where
  deserialize = toArgBody
  serialize = fromArgBody

----------------------------------------
-- DefunMatch
----------------------------------------

nameDefunMatch :: NameSymbol.T
nameDefunMatch = ":defun-match"

isDefunMatch :: Sexp.T -> Bool
isDefunMatch (Sexp.Cons form _) = Sexp.isAtomNamed form nameDefunMatch
isDefunMatch _ = False

toDefunMatch :: Sexp.T -> Maybe DefunMatch
toDefunMatch form
  | isDefunMatch form =
    case form of
      _nameDefunMatch Sexp.:> sexp1 Sexp.:> argBody2
        | Just argBody2 <- toArgBody `fromStarList` argBody2 ->
          DefunMatch sexp1 argBody2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDefunMatch :: DefunMatch -> Sexp.T
fromDefunMatch (DefunMatch sexp1 argBody2) =
  Sexp.listStar [Sexp.atom nameDefunMatch, sexp1, fromArgBody `toStarList` argBody2]

instance Sexp.Serialize DefunMatch where
  deserialize = toDefunMatch
  serialize = fromDefunMatch

----------------------------------------
-- If
----------------------------------------

nameIf :: NameSymbol.T
nameIf = "if"

isIf :: Sexp.T -> Bool
isIf (Sexp.Cons form _) = Sexp.isAtomNamed form nameIf
isIf _ = False

toIf :: Sexp.T -> Maybe If
toIf form
  | isIf form =
    case form of
      _nameIf Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> Sexp.Nil ->
        If sexp1 sexp2 sexp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromIf :: If -> Sexp.T
fromIf (If sexp1 sexp2 sexp3) =
  Sexp.list [Sexp.atom nameIf, sexp1, sexp2, sexp3]

instance Sexp.Serialize If where
  deserialize = toIf
  serialize = fromIf

----------------------------------------
-- IfNoElse
----------------------------------------

nameIfNoElse :: NameSymbol.T
nameIfNoElse = "if"

isIfNoElse :: Sexp.T -> Bool
isIfNoElse (Sexp.Cons form _) = Sexp.isAtomNamed form nameIfNoElse
isIfNoElse _ = False

toIfNoElse :: Sexp.T -> Maybe IfNoElse
toIfNoElse form
  | isIfNoElse form =
    case form of
      _nameIfNoElse Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        IfNoElse sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromIfNoElse :: IfNoElse -> Sexp.T
fromIfNoElse (IfNoElse sexp1 sexp2) =
  Sexp.list [Sexp.atom nameIfNoElse, sexp1, sexp2]

instance Sexp.Serialize IfNoElse where
  deserialize = toIfNoElse
  serialize = fromIfNoElse

----------------------------------------
-- DefunSigMatch
----------------------------------------

nameDefunSigMatch :: NameSymbol.T
nameDefunSigMatch = ":defsig-match"

isDefunSigMatch :: Sexp.T -> Bool
isDefunSigMatch (Sexp.Cons form _) = Sexp.isAtomNamed form nameDefunSigMatch
isDefunSigMatch _ = False

toDefunSigMatch :: Sexp.T -> Maybe DefunSigMatch
toDefunSigMatch form
  | isDefunSigMatch form =
    case form of
      _nameDefunSigMatch Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> argBody3
        | Just argBody3 <- toArgBody `fromStarList` argBody3 ->
          DefunSigMatch sexp1 sexp2 argBody3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDefunSigMatch :: DefunSigMatch -> Sexp.T
fromDefunSigMatch (DefunSigMatch sexp1 sexp2 argBody3) =
  Sexp.listStar [Sexp.atom nameDefunSigMatch, sexp1, sexp2, fromArgBody `toStarList` argBody3]

instance Sexp.Serialize DefunSigMatch where
  deserialize = toDefunSigMatch
  serialize = fromDefunSigMatch

----------------------------------------
-- LetMatch
----------------------------------------

nameLetMatch :: NameSymbol.T
nameLetMatch = ":let-match"

isLetMatch :: Sexp.T -> Bool
isLetMatch (Sexp.Cons form _) = Sexp.isAtomNamed form nameLetMatch
isLetMatch _ = False

toLetMatch :: Sexp.T -> Maybe LetMatch
toLetMatch form
  | isLetMatch form =
    case form of
      _nameLetMatch Sexp.:> sexp1 Sexp.:> argBodys2 Sexp.:> sexp3 Sexp.:> Sexp.Nil
        | Just argBodys2 <- toArgBodys argBodys2 ->
          LetMatch sexp1 argBodys2 sexp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLetMatch :: LetMatch -> Sexp.T
fromLetMatch (LetMatch sexp1 argBodys2 sexp3) =
  Sexp.list [Sexp.atom nameLetMatch, sexp1, fromArgBodys argBodys2, sexp3]

instance Sexp.Serialize LetMatch where
  deserialize = toLetMatch
  serialize = fromLetMatch

----------------------------------------
-- RecordNoPunned
----------------------------------------

nameRecordNoPunned :: NameSymbol.T
nameRecordNoPunned = ":record-no-pun"

isRecordNoPunned :: Sexp.T -> Bool
isRecordNoPunned (Sexp.Cons form _) = Sexp.isAtomNamed form nameRecordNoPunned
isRecordNoPunned _ = False

toRecordNoPunned :: Sexp.T -> Maybe RecordNoPunned
toRecordNoPunned form
  | isRecordNoPunned form =
    case form of
      _nameRecordNoPunned Sexp.:> notPunnedGroup1
        | Just notPunnedGroup1 <- toNotPunnedGroup notPunnedGroup1 ->
          RecordNoPunned notPunnedGroup1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromRecordNoPunned :: RecordNoPunned -> Sexp.T
fromRecordNoPunned (RecordNoPunned notPunnedGroup1) =
  Sexp.listStar [Sexp.atom nameRecordNoPunned, fromNotPunnedGroup notPunnedGroup1]

instance Sexp.Serialize RecordNoPunned where
  deserialize = toRecordNoPunned
  serialize = fromRecordNoPunned

----------------------------------------
-- LambdaCase
----------------------------------------

nameLambdaCase :: NameSymbol.T
nameLambdaCase = ":lambda-case"

isLambdaCase :: Sexp.T -> Bool
isLambdaCase (Sexp.Cons form _) = Sexp.isAtomNamed form nameLambdaCase
isLambdaCase _ = False

toLambdaCase :: Sexp.T -> Maybe LambdaCase
toLambdaCase form
  | isLambdaCase form =
    case form of
      _nameLambdaCase Sexp.:> argBody1
        | Just argBody1 <- toArgBody `fromStarList` argBody1 ->
          LambdaCase argBody1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLambdaCase :: LambdaCase -> Sexp.T
fromLambdaCase (LambdaCase argBody1) =
  Sexp.listStar [Sexp.atom nameLambdaCase, fromArgBody `toStarList` argBody1]

instance Sexp.Serialize LambdaCase where
  deserialize = toLambdaCase
  serialize = fromLambdaCase

----------------------------------------
-- LetHandler
----------------------------------------

nameLetHandler :: NameSymbol.T
nameLetHandler = ":lethandler"

isLetHandler :: Sexp.T -> Bool
isLetHandler (Sexp.Cons form _) = Sexp.isAtomNamed form nameLetHandler
isLetHandler _ = False

toLetHandler :: Sexp.T -> Maybe LetHandler
toLetHandler form
  | isLetHandler form =
    case form of
      _nameLetHandler Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> Sexp.Nil ->
        LetHandler sexp1 sexp2 sexp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLetHandler :: LetHandler -> Sexp.T
fromLetHandler (LetHandler sexp1 sexp2 sexp3) =
  Sexp.list [Sexp.atom nameLetHandler, sexp1, sexp2, sexp3]

instance Sexp.Serialize LetHandler where
  deserialize = toLetHandler
  serialize = fromLetHandler

----------------------------------------
-- Handler
----------------------------------------

nameHandler :: NameSymbol.T
nameHandler = ":lethandler"

isHandler :: Sexp.T -> Bool
isHandler (Sexp.Cons form _) = Sexp.isAtomNamed form nameHandler
isHandler _ = False

toHandler :: Sexp.T -> Maybe Handler
toHandler form
  | isHandler form =
    case form of
      _nameHandler Sexp.:> sexp1 Sexp.:> letRet2 Sexp.:> letOp3
        | Just letRet2 <- toLetRet letRet2,
          Just letOp3 <- toLetOp `fromStarList` letOp3 ->
          Handler sexp1 letRet2 letOp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromHandler :: Handler -> Sexp.T
fromHandler (Handler sexp1 letRet2 letOp3) =
  Sexp.listStar [Sexp.atom nameHandler, sexp1, fromLetRet letRet2, fromLetOp `toStarList` letOp3]

instance Sexp.Serialize Handler where
  deserialize = toHandler
  serialize = fromHandler

----------------------------------------
-- SumCon
----------------------------------------

nameSumCon :: NameSymbol.T
nameSumCon = ":sum-con"

isSumCon :: Sexp.T -> Bool
isSumCon (Sexp.Cons form _) = Sexp.isAtomNamed form nameSumCon
isSumCon _ = False

toSumCon :: Sexp.T -> Maybe SumCon
toSumCon form
  | isSumCon form =
    case form of
      _nameSumCon Sexp.:> nameSymbol1 Sexp.:> Sexp.Nil
        | Just nameSymbol1 <- toNameSymbol nameSymbol1 ->
          SumCon nameSymbol1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromSumCon :: SumCon -> Sexp.T
fromSumCon (SumCon nameSymbol1) =
  Sexp.list [Sexp.atom nameSumCon, fromNameSymbol nameSymbol1]

instance Sexp.Serialize SumCon where
  deserialize = toSumCon
  serialize = fromSumCon

----------------------------------------
-- SumConFilled
----------------------------------------

nameSumConFilled :: NameSymbol.T
nameSumConFilled = ":sum-con-filled"

isSumConFilled :: Sexp.T -> Bool
isSumConFilled (Sexp.Cons form _) = Sexp.isAtomNamed form nameSumConFilled
isSumConFilled _ = False

toSumConFilled :: Sexp.T -> Maybe SumConFilled
toSumConFilled form
  | isSumConFilled form =
    case form of
      _nameSumConFilled Sexp.:> nameSymbol1 Sexp.:> sexp2 Sexp.:> Sexp.Nil
        | Just nameSymbol1 <- toNameSymbol nameSymbol1 ->
          SumConFilled nameSymbol1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromSumConFilled :: SumConFilled -> Sexp.T
fromSumConFilled (SumConFilled nameSymbol1 sexp2) =
  Sexp.list [Sexp.atom nameSumConFilled, fromNameSymbol nameSymbol1, sexp2]

instance Sexp.Serialize SumConFilled where
  deserialize = toSumConFilled
  serialize = fromSumConFilled

----------------------------------------
-- InPackage
----------------------------------------

nameInPackage :: NameSymbol.T
nameInPackage = ":in-package"

isInPackage :: Sexp.T -> Bool
isInPackage (Sexp.Cons form _) = Sexp.isAtomNamed form nameInPackage
isInPackage _ = False

toInPackage :: Sexp.T -> Maybe InPackage
toInPackage form
  | isInPackage form =
    case form of
      _nameInPackage Sexp.:> nameSymbol1 Sexp.:> Sexp.Nil
        | Just nameSymbol1 <- toNameSymbol nameSymbol1 ->
          InPackage nameSymbol1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromInPackage :: InPackage -> Sexp.T
fromInPackage (InPackage nameSymbol1) =
  Sexp.list [Sexp.atom nameInPackage, fromNameSymbol nameSymbol1]

instance Sexp.Serialize InPackage where
  deserialize = toInPackage
  serialize = fromInPackage
