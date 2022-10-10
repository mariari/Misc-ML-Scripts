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
module Data.Sexp.Structure.Parsing where

import Mari.Library hiding (Type)
import qualified Mari.Library.NameSymbol as NameSymbol
import qualified Data.Sexp as Sexp
import Data.Sexp.Structure.Helpers

-- | @Defun@ is the base defun structure
-- currently it does not have matching
data Defun = Defun
  { defunName :: Sexp.T,
    defunArgs :: Sexp.T,
    defunBody :: Sexp.T
  }
  deriving (Show)

data Include = Include
  { includeName :: NameSymbol.T
  }
  deriving (Show)

data Alias = Alias
  { aliasName :: NameSymbol.T,
    aliasModuleOf :: NameSymbol.T
  }
  deriving (Show)

-- | @Type@ is the type declaration structure
data Type = Type
  { -- TODO ∷ we should really have a field the signature currently we
    -- don't really support that without doing it by hand. Maybe our
    -- generator should help here, or we should have a way of talking
    -- about named arguments somehow, and unfolding the slots in a
    -- way....
    typeNameAndSig :: Sexp.T,
    typeArgs :: Sexp.T,
    typeBody :: Sexp.T
  }
  deriving (Show)

-- | @PredAns@ is an abstraction over questions and answers
data PredAns = PredAns {predAnsPredicate :: Sexp.T, predAnsAnswer :: Sexp.T}
  deriving (Show)

-- | @Cond@ here Cond form takes a list of predicate answers
newtype Cond = Cond {condEntailments :: [PredAns]} deriving (Show)

-- | @Signature@ is the signature of the term
data Signature = Signature
  { signatureName :: Sexp.T,
    signatureSig :: Sexp.T
  }
  deriving (Show)

data LetSignature = LetSignature
  { letSignatureName :: Sexp.T,
    letSignatureSig :: Sexp.T,
    letSignatureRest :: Sexp.T
  }
  deriving (Show)

-- | @LetType@ is the let-type form of the language
data LetType = LetType
  { letTypeNameAndSig :: Sexp.T,
    letTypeArgs :: Sexp.T,
    letTypeBody :: Sexp.T,
    letTypeRest :: Sexp.T
  }
  deriving (Show)

-- | @Let@ is the let form of the language
-- it has a name, arguments, body, and the body
data Let = Let
  { letName :: Sexp.T,
    letArgs :: Sexp.T,
    letBody :: Sexp.T,
    letRest :: Sexp.T
  }
  deriving (Show)

data Case = Case
  { caseOn :: Sexp.T,
    caseImplications :: [DeconBody]
  }
  deriving (Show)

-- | @DeconBody@ is an abstraction over a matching body form
data DeconBody = DeconBody
  { deconBodyDeconsturctor :: Sexp.T,
    deconBodyBody :: Sexp.T
  }
  deriving (Show)

data Arrow = Arrow
  { arrowName :: Sexp.T,
    arrowBody :: Sexp.T
  }
  deriving (Show)

data Lambda = Lambda
  { lambdaArgs :: Sexp.T,
    lambdaBody :: Sexp.T
  }
  deriving (Show)

-- | @NameBind@ represents a type naming scheme in a record
data NameBind = Pun Punned | NotPun NotPunned
  deriving (Show)

-- | @NotPunned@ represents a punned type name in a record
data NotPunned = NotPunned
  { notPunnedName :: Sexp.T,
    notPunnedValue :: Sexp.T
  }
  deriving (Show)

data NameUsage = NameUsage
  { nameUsageName :: Sexp.T,
    nameUsageUsage :: Sexp.T,
    nameUsageValue :: Sexp.T
  }
  deriving (Show)

newtype Punned = Punned
  { punnedName :: Sexp.T
  }
  deriving (Show)

newtype Record = Record
  { recordValue :: [NameBind]
  }
  deriving (Show)

newtype RecordDec = RecordDec
  { recordDecValue :: [NameUsage]
  }
  deriving (Show)

-- | @Infix@ represents an infix function
data Infix = Infix
  { infixOp :: Sexp.T,
    infixLeft :: Sexp.T,
    infixRight :: Sexp.T
  }
  deriving (Show)

newtype Open = Open
  { openName :: Sexp.T
  }
  deriving (Show)

data OpenIn = OpenIn
  { openInName :: Sexp.T,
    openInBody :: Sexp.T
  }
  deriving (Show)

newtype Declare = Declare
  { declareClaim :: Sexp.T
  }
  deriving (Show)

data Declaim = Declaim
  { declaimClaim :: Sexp.T,
    declaimBody :: Sexp.T
  }
  deriving (Show)

-- | @DefModule@ - Stands in for a module declaration
data DefModule = DefModule
  { defModuleName :: Sexp.T,
    defModuleArgs :: Sexp.T,
    defModuleBody :: Sexp.T
  }
  deriving (Show)

-- | @LefModule@ - Stands in for a module let declaration
data LetModule = LetModule
  { letModuleName :: Sexp.T,
    letModuleArgs :: Sexp.T,
    letModuleBody :: Sexp.T,
    letModuleRest :: Sexp.T
  }
  deriving (Show)

newtype Primitive = Primitive
  { primitiveName :: Sexp.T
  }
  deriving (Show)

data Effect = Effect
  { effectName :: Sexp.T,
    effectOps :: Sexp.T
  }
  deriving (Show)

data DefHandler = DefHandler
  { defHandlerName :: Sexp.T,
    defHandlerOps :: Sexp.T
  }
  deriving (Show)

data LetOp = LetOp
  { letOpName :: Sexp.T,
    letOpArgs :: Sexp.T,
    letOpBody :: Sexp.T
  }
  deriving (Show)

data LetRet = LetRet
  { letRetArg :: Sexp.T,
    letRetBody :: Sexp.T
  }
  deriving (Show)

newtype Do = Do
  { doStatements :: Sexp.T
  }
  deriving (Show)

newtype DoDeep = DoDeep
  { doDeepStatements :: [DoBodyFull]
  }
  deriving (Show)

data DoBodyFull
  = WithBinder
      { doBodyFullName :: NameSymbol.T,
        doBodyFullBBody :: Sexp.T
      }
  | NoBinder {doBodyFullBody :: Sexp.T}
  deriving (Show)

--

-- Used to transform into WithBinder
data Binder = Binder
  { binderName :: NameSymbol.T,
    binderBody :: Sexp.T
  }
  deriving (Show)

-- Used to for NoBinder
newtype DoBody = DoBody {doBodySexp :: Sexp.T}

data DoOp = DoOp
  { doOpName :: Sexp.T,
    doOpArgs :: Sexp.T
  }
  deriving (Show)

newtype DoPure = DoPure
  { doPureArg :: Sexp.T
  }
  deriving (Show)

data Via = Via
  { viaHandler :: Sexp.T,
    viaProgram :: Sexp.T
  }
  deriving (Show)

data Header = Header
  { headerName :: NameSymbol.T,
    headerSexps :: Sexp.T
  }
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

--------------------
-- NotPunned no Grouping
--------------------
fromNotPunnedGroup :: [NotPunned] -> Sexp.T
fromNotPunnedGroup = Sexp.unGroupBy2 . toStarList fromNotPunned

toNotPunnedGroup :: Sexp.T -> Maybe [NotPunned]
toNotPunnedGroup = fromStarList toNotPunned . Sexp.groupBy2

--------------------
-- Name Bind
--------------------

toNameBind :: Sexp.T -> Maybe NameBind
toNameBind sexp =
  fmap Pun (toPunned sexp) <|> fmap NotPun (toNotPunned sexp)

fromNameBind :: NameBind -> Sexp.T
fromNameBind (Pun pun) = fromPunned pun
fromNameBind (NotPun notPun) = fromNotPunned notPun

instance Sexp.Serialize NameBind where
  deserialize = toNameBind
  serialize = fromNameBind

--------------------
-- Do Body Full
--------------------

-- TODO ∷ Change the frontend generator, as this does not fulfill it.
toDoBodyFull :: Sexp.T -> Maybe DoBodyFull
toDoBodyFull sexp =
  fmap binderToBody (toBinder sexp) <|> Just (NoBinder sexp)

fromDoBodyFull :: DoBodyFull -> Sexp.T
fromDoBodyFull (NoBinder sexp) = sexp
fromDoBodyFull (WithBinder name body) = Binder name body |> fromBinder

binderToBody :: Binder -> DoBodyFull
binderToBody (Binder name body) = WithBinder name body

instance Sexp.Serialize DoBodyFull where
  deserialize = toDoBodyFull
  serialize = fromDoBodyFull

----------------------------------------
-- Generated
----------------------------------------

----------------------------------------
-- Type
----------------------------------------

nameType :: NameSymbol.T
nameType = "type"

isType :: Sexp.T -> Bool
isType (Sexp.Cons form _) = Sexp.isAtomNamed form nameType
isType _ = False

toType :: Sexp.T -> Maybe Type
toType form
  | isType form =
    case form of
      _nameType Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 ->
        Type sexp1 sexp2 sexp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromType :: Type -> Sexp.T
fromType (Type sexp1 sexp2 sexp3) =
  Sexp.listStar [Sexp.atom nameType, sexp1, sexp2, sexp3]

instance Sexp.Serialize Type where
  deserialize = toType
  serialize = fromType

----------------------------------------
-- LetType
----------------------------------------

nameLetType :: NameSymbol.T
nameLetType = ":let-type"

isLetType :: Sexp.T -> Bool
isLetType (Sexp.Cons form _) = Sexp.isAtomNamed form nameLetType
isLetType _ = False

toLetType :: Sexp.T -> Maybe LetType
toLetType form
  | isLetType form =
    case form of
      _nameLetType Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> sexp4 Sexp.:> Sexp.Nil ->
        LetType sexp1 sexp2 sexp3 sexp4 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLetType :: LetType -> Sexp.T
fromLetType (LetType sexp1 sexp2 sexp3 sexp4) =
  Sexp.list [Sexp.atom nameLetType, sexp1, sexp2, sexp3, sexp4]

instance Sexp.Serialize LetType where
  deserialize = toLetType
  serialize = fromLetType

----------------------------------------
-- Include
----------------------------------------

nameInclude :: NameSymbol.T
nameInclude = ":include"

isInclude :: Sexp.T -> Bool
isInclude (Sexp.Cons form _) = Sexp.isAtomNamed form nameInclude
isInclude _ = False

toInclude :: Sexp.T -> Maybe Include
toInclude form
  | isInclude form =
    case form of
      _nameInclude Sexp.:> nameSymbol1 Sexp.:> Sexp.Nil
        | Just nameSymbol1 <- toNameSymbol nameSymbol1 ->
          Include nameSymbol1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromInclude :: Include -> Sexp.T
fromInclude (Include nameSymbol1) =
  Sexp.list [Sexp.atom nameInclude, fromNameSymbol nameSymbol1]

instance Sexp.Serialize Include where
  deserialize = toInclude
  serialize = fromInclude

----------------------------------------
-- Alias
----------------------------------------

nameAlias :: NameSymbol.T
nameAlias = ":alias"

isAlias :: Sexp.T -> Bool
isAlias (Sexp.Cons form _) = Sexp.isAtomNamed form nameAlias
isAlias _ = False

toAlias :: Sexp.T -> Maybe Alias
toAlias form
  | isAlias form =
    case form of
      _nameAlias Sexp.:> nameSymbol1 Sexp.:> nameSymbol2 Sexp.:> Sexp.Nil
        | Just nameSymbol1 <- toNameSymbol nameSymbol1,
          Just nameSymbol2 <- toNameSymbol nameSymbol2 ->
          Alias nameSymbol1 nameSymbol2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromAlias :: Alias -> Sexp.T
fromAlias (Alias nameSymbol1 nameSymbol2) =
  Sexp.list [Sexp.atom nameAlias, fromNameSymbol nameSymbol1, fromNameSymbol nameSymbol2]

instance Sexp.Serialize Alias where
  deserialize = toAlias
  serialize = fromAlias

----------------------------------------
-- Defun
----------------------------------------

nameDefun :: NameSymbol.T
nameDefun = ":defun"

isDefun :: Sexp.T -> Bool
isDefun (Sexp.Cons form _) = Sexp.isAtomNamed form nameDefun
isDefun _ = False

toDefun :: Sexp.T -> Maybe Defun
toDefun form
  | isDefun form =
    case form of
      _nameDefun Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> Sexp.Nil ->
        Defun sexp1 sexp2 sexp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDefun :: Defun -> Sexp.T
fromDefun (Defun sexp1 sexp2 sexp3) =
  Sexp.list [Sexp.atom nameDefun, sexp1, sexp2, sexp3]

instance Sexp.Serialize Defun where
  deserialize = toDefun
  serialize = fromDefun

----------------------------------------
-- Signature
----------------------------------------

nameSignature :: NameSymbol.T
nameSignature = ":defsig"

isSignature :: Sexp.T -> Bool
isSignature (Sexp.Cons form _) = Sexp.isAtomNamed form nameSignature
isSignature _ = False

toSignature :: Sexp.T -> Maybe Signature
toSignature form
  | isSignature form =
    case form of
      _nameSignature Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        Signature sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromSignature :: Signature -> Sexp.T
fromSignature (Signature sexp1 sexp2) =
  Sexp.list [Sexp.atom nameSignature, sexp1, sexp2]

instance Sexp.Serialize Signature where
  deserialize = toSignature
  serialize = fromSignature

----------------------------------------
-- LetSignature
----------------------------------------

nameLetSignature :: NameSymbol.T
nameLetSignature = ":let-sig"

isLetSignature :: Sexp.T -> Bool
isLetSignature (Sexp.Cons form _) = Sexp.isAtomNamed form nameLetSignature
isLetSignature _ = False

toLetSignature :: Sexp.T -> Maybe LetSignature
toLetSignature form
  | isLetSignature form =
    case form of
      _nameLetSignature Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> Sexp.Nil ->
        LetSignature sexp1 sexp2 sexp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLetSignature :: LetSignature -> Sexp.T
fromLetSignature (LetSignature sexp1 sexp2 sexp3) =
  Sexp.list [Sexp.atom nameLetSignature, sexp1, sexp2, sexp3]

instance Sexp.Serialize LetSignature where
  deserialize = toLetSignature
  serialize = fromLetSignature

----------------------------------------
-- Let
----------------------------------------

nameLet :: NameSymbol.T
nameLet = "let"

isLet :: Sexp.T -> Bool
isLet (Sexp.Cons form _) = Sexp.isAtomNamed form nameLet
isLet _ = False

toLet :: Sexp.T -> Maybe Let
toLet form
  | isLet form =
    case form of
      _nameLet Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> sexp4 Sexp.:> Sexp.Nil ->
        Let sexp1 sexp2 sexp3 sexp4 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLet :: Let -> Sexp.T
fromLet (Let sexp1 sexp2 sexp3 sexp4) =
  Sexp.list [Sexp.atom nameLet, sexp1, sexp2, sexp3, sexp4]

instance Sexp.Serialize Let where
  deserialize = toLet
  serialize = fromLet

----------------------------------------
-- PredAns
----------------------------------------

toPredAns :: Sexp.T -> Maybe PredAns
toPredAns form =
  case form of
    sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
      PredAns sexp1 sexp2 |> Just
    _ ->
      Nothing

fromPredAns :: PredAns -> Sexp.T
fromPredAns (PredAns sexp1 sexp2) =
  Sexp.list [sexp1, sexp2]

instance Sexp.Serialize PredAns where
  deserialize = toPredAns
  serialize = fromPredAns

----------------------------------------
-- Cond
----------------------------------------

nameCond :: NameSymbol.T
nameCond = ":cond"

isCond :: Sexp.T -> Bool
isCond (Sexp.Cons form _) = Sexp.isAtomNamed form nameCond
isCond _ = False

toCond :: Sexp.T -> Maybe Cond
toCond form
  | isCond form =
    case form of
      _nameCond Sexp.:> predAns1
        | Just predAns1 <- toPredAns `fromStarList` predAns1 ->
          Cond predAns1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromCond :: Cond -> Sexp.T
fromCond (Cond predAns1) =
  Sexp.listStar [Sexp.atom nameCond, fromPredAns `toStarList` predAns1]

instance Sexp.Serialize Cond where
  deserialize = toCond
  serialize = fromCond

----------------------------------------
-- DeconBody
----------------------------------------

toDeconBody :: Sexp.T -> Maybe DeconBody
toDeconBody form =
  case form of
    sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
      DeconBody sexp1 sexp2 |> Just
    _ ->
      Nothing

fromDeconBody :: DeconBody -> Sexp.T
fromDeconBody (DeconBody sexp1 sexp2) =
  Sexp.list [sexp1, sexp2]

instance Sexp.Serialize DeconBody where
  deserialize = toDeconBody
  serialize = fromDeconBody

----------------------------------------
-- Case
----------------------------------------

nameCase :: NameSymbol.T
nameCase = "case"

isCase :: Sexp.T -> Bool
isCase (Sexp.Cons form _) = Sexp.isAtomNamed form nameCase
isCase _ = False

toCase :: Sexp.T -> Maybe Case
toCase form
  | isCase form =
    case form of
      _nameCase Sexp.:> sexp1 Sexp.:> deconBody2
        | Just deconBody2 <- toDeconBody `fromStarList` deconBody2 ->
          Case sexp1 deconBody2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromCase :: Case -> Sexp.T
fromCase (Case sexp1 deconBody2) =
  Sexp.listStar [Sexp.atom nameCase, sexp1, fromDeconBody `toStarList` deconBody2]

instance Sexp.Serialize Case where
  deserialize = toCase
  serialize = fromCase

----------------------------------------
-- Arrow
----------------------------------------

nameArrow :: NameSymbol.T
nameArrow = "%<-"

isArrow :: Sexp.T -> Bool
isArrow (Sexp.Cons form _) = Sexp.isAtomNamed form nameArrow
isArrow _ = False

toArrow :: Sexp.T -> Maybe Arrow
toArrow form
  | isArrow form =
    case form of
      _nameArrow Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        Arrow sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromArrow :: Arrow -> Sexp.T
fromArrow (Arrow sexp1 sexp2) =
  Sexp.list [Sexp.atom nameArrow, sexp1, sexp2]

instance Sexp.Serialize Arrow where
  deserialize = toArrow
  serialize = fromArrow

----------------------------------------
-- Lambda
----------------------------------------

nameLambda :: NameSymbol.T
nameLambda = ":lambda"

isLambda :: Sexp.T -> Bool
isLambda (Sexp.Cons form _) = Sexp.isAtomNamed form nameLambda
isLambda _ = False

toLambda :: Sexp.T -> Maybe Lambda
toLambda form
  | isLambda form =
    case form of
      _nameLambda Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        Lambda sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLambda :: Lambda -> Sexp.T
fromLambda (Lambda sexp1 sexp2) =
  Sexp.list [Sexp.atom nameLambda, sexp1, sexp2]

instance Sexp.Serialize Lambda where
  deserialize = toLambda
  serialize = fromLambda

----------------------------------------
-- Punned
----------------------------------------

toPunned :: Sexp.T -> Maybe Punned
toPunned form =
  case form of
    sexp1 Sexp.:> Sexp.Nil ->
      Punned sexp1 |> Just
    _ ->
      Nothing

fromPunned :: Punned -> Sexp.T
fromPunned (Punned sexp1) =
  Sexp.list [sexp1]

instance Sexp.Serialize Punned where
  deserialize = toPunned
  serialize = fromPunned

----------------------------------------
-- NotPunned
----------------------------------------

toNotPunned :: Sexp.T -> Maybe NotPunned
toNotPunned form =
  case form of
    sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
      NotPunned sexp1 sexp2 |> Just
    _ ->
      Nothing

fromNotPunned :: NotPunned -> Sexp.T
fromNotPunned (NotPunned sexp1 sexp2) =
  Sexp.list [sexp1, sexp2]

instance Sexp.Serialize NotPunned where
  deserialize = toNotPunned
  serialize = fromNotPunned

----------------------------------------
-- NameUsage
----------------------------------------

toNameUsage :: Sexp.T -> Maybe NameUsage
toNameUsage form =
  case form of
    sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> Sexp.Nil ->
      NameUsage sexp1 sexp2 sexp3 |> Just
    _ ->
      Nothing

fromNameUsage :: NameUsage -> Sexp.T
fromNameUsage (NameUsage sexp1 sexp2 sexp3) =
  Sexp.list [sexp1, sexp2, sexp3]

instance Sexp.Serialize NameUsage where
  deserialize = toNameUsage
  serialize = fromNameUsage

----------------------------------------
-- Record
----------------------------------------

nameRecord :: NameSymbol.T
nameRecord = ":record"

isRecord :: Sexp.T -> Bool
isRecord (Sexp.Cons form _) = Sexp.isAtomNamed form nameRecord
isRecord _ = False

toRecord :: Sexp.T -> Maybe Record
toRecord form
  | isRecord form =
    case form of
      _nameRecord Sexp.:> nameBind1
        | Just nameBind1 <- toNameBind `fromStarList` nameBind1 ->
          Record nameBind1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromRecord :: Record -> Sexp.T
fromRecord (Record nameBind1) =
  Sexp.listStar [Sexp.atom nameRecord, fromNameBind `toStarList` nameBind1]

instance Sexp.Serialize Record where
  deserialize = toRecord
  serialize = fromRecord

----------------------------------------
-- Infix
----------------------------------------

nameInfix :: NameSymbol.T
nameInfix = ":infix"

isInfix :: Sexp.T -> Bool
isInfix (Sexp.Cons form _) = Sexp.isAtomNamed form nameInfix
isInfix _ = False

toInfix :: Sexp.T -> Maybe Infix
toInfix form
  | isInfix form =
    case form of
      _nameInfix Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> Sexp.Nil ->
        Infix sexp1 sexp2 sexp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromInfix :: Infix -> Sexp.T
fromInfix (Infix sexp1 sexp2 sexp3) =
  Sexp.list [Sexp.atom nameInfix, sexp1, sexp2, sexp3]

instance Sexp.Serialize Infix where
  deserialize = toInfix
  serialize = fromInfix

----------------------------------------
-- OpenIn
----------------------------------------

nameOpenIn :: NameSymbol.T
nameOpenIn = ":open-in"

isOpenIn :: Sexp.T -> Bool
isOpenIn (Sexp.Cons form _) = Sexp.isAtomNamed form nameOpenIn
isOpenIn _ = False

toOpenIn :: Sexp.T -> Maybe OpenIn
toOpenIn form
  | isOpenIn form =
    case form of
      _nameOpenIn Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        OpenIn sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromOpenIn :: OpenIn -> Sexp.T
fromOpenIn (OpenIn sexp1 sexp2) =
  Sexp.list [Sexp.atom nameOpenIn, sexp1, sexp2]

instance Sexp.Serialize OpenIn where
  deserialize = toOpenIn
  serialize = fromOpenIn

----------------------------------------
-- Open
----------------------------------------

nameOpen :: NameSymbol.T
nameOpen = "open"

isOpen :: Sexp.T -> Bool
isOpen (Sexp.Cons form _) = Sexp.isAtomNamed form nameOpen
isOpen _ = False

toOpen :: Sexp.T -> Maybe Open
toOpen form
  | isOpen form =
    case form of
      _nameOpen Sexp.:> sexp1 Sexp.:> Sexp.Nil ->
        Open sexp1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromOpen :: Open -> Sexp.T
fromOpen (Open sexp1) =
  Sexp.list [Sexp.atom nameOpen, sexp1]

instance Sexp.Serialize Open where
  deserialize = toOpen
  serialize = fromOpen

----------------------------------------
-- Declare
----------------------------------------

nameDeclare :: NameSymbol.T
nameDeclare = "declare"

isDeclare :: Sexp.T -> Bool
isDeclare (Sexp.Cons form _) = Sexp.isAtomNamed form nameDeclare
isDeclare _ = False

toDeclare :: Sexp.T -> Maybe Declare
toDeclare form
  | isDeclare form =
    case form of
      _nameDeclare Sexp.:> sexp1 Sexp.:> Sexp.Nil ->
        Declare sexp1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDeclare :: Declare -> Sexp.T
fromDeclare (Declare sexp1) =
  Sexp.list [Sexp.atom nameDeclare, sexp1]

instance Sexp.Serialize Declare where
  deserialize = toDeclare
  serialize = fromDeclare

----------------------------------------
-- Declaim
----------------------------------------

nameDeclaim :: NameSymbol.T
nameDeclaim = ":declaim"

isDeclaim :: Sexp.T -> Bool
isDeclaim (Sexp.Cons form _) = Sexp.isAtomNamed form nameDeclaim
isDeclaim _ = False

toDeclaim :: Sexp.T -> Maybe Declaim
toDeclaim form
  | isDeclaim form =
    case form of
      _nameDeclaim Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        Declaim sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDeclaim :: Declaim -> Sexp.T
fromDeclaim (Declaim sexp1 sexp2) =
  Sexp.list [Sexp.atom nameDeclaim, sexp1, sexp2]

instance Sexp.Serialize Declaim where
  deserialize = toDeclaim
  serialize = fromDeclaim

----------------------------------------
-- DefModule
----------------------------------------

nameDefModule :: NameSymbol.T
nameDefModule = ":defmodule"

isDefModule :: Sexp.T -> Bool
isDefModule (Sexp.Cons form _) = Sexp.isAtomNamed form nameDefModule
isDefModule _ = False

toDefModule :: Sexp.T -> Maybe DefModule
toDefModule form
  | isDefModule form =
    case form of
      _nameDefModule Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 ->
        DefModule sexp1 sexp2 sexp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDefModule :: DefModule -> Sexp.T
fromDefModule (DefModule sexp1 sexp2 sexp3) =
  Sexp.listStar [Sexp.atom nameDefModule, sexp1, sexp2, sexp3]

instance Sexp.Serialize DefModule where
  deserialize = toDefModule
  serialize = fromDefModule

----------------------------------------
-- Do
----------------------------------------

nameDo :: NameSymbol.T
nameDo = ":do"

isDo :: Sexp.T -> Bool
isDo (Sexp.Cons form _) = Sexp.isAtomNamed form nameDo
isDo _ = False

toDo :: Sexp.T -> Maybe Do
toDo form
  | isDo form =
    case form of
      _nameDo Sexp.:> sexp1 ->
        Do sexp1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDo :: Do -> Sexp.T
fromDo (Do sexp1) =
  Sexp.listStar [Sexp.atom nameDo, sexp1]

instance Sexp.Serialize Do where
  deserialize = toDo
  serialize = fromDo

----------------------------------------
-- LetModule
----------------------------------------

nameLetModule :: NameSymbol.T
nameLetModule = ":let-mod"

isLetModule :: Sexp.T -> Bool
isLetModule (Sexp.Cons form _) = Sexp.isAtomNamed form nameLetModule
isLetModule _ = False

toLetModule :: Sexp.T -> Maybe LetModule
toLetModule form
  | isLetModule form =
    case form of
      _nameLetModule Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> sexp4 Sexp.:> Sexp.Nil ->
        LetModule sexp1 sexp2 sexp3 sexp4 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLetModule :: LetModule -> Sexp.T
fromLetModule (LetModule sexp1 sexp2 sexp3 sexp4) =
  Sexp.list [Sexp.atom nameLetModule, sexp1, sexp2, sexp3, sexp4]

instance Sexp.Serialize LetModule where
  deserialize = toLetModule
  serialize = fromLetModule

----------------------------------------
-- Effect
----------------------------------------

nameEffect :: NameSymbol.T
nameEffect = ":defeff"

isEffect :: Sexp.T -> Bool
isEffect (Sexp.Cons form _) = Sexp.isAtomNamed form nameEffect
isEffect _ = False

toEffect :: Sexp.T -> Maybe Effect
toEffect form
  | isEffect form =
    case form of
      _nameEffect Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        Effect sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromEffect :: Effect -> Sexp.T
fromEffect (Effect sexp1 sexp2) =
  Sexp.list [Sexp.atom nameEffect, sexp1, sexp2]

instance Sexp.Serialize Effect where
  deserialize = toEffect
  serialize = fromEffect

----------------------------------------
-- DefHandler
----------------------------------------

nameDefHandler :: NameSymbol.T
nameDefHandler = ":defhandler"

isDefHandler :: Sexp.T -> Bool
isDefHandler (Sexp.Cons form _) = Sexp.isAtomNamed form nameDefHandler
isDefHandler _ = False

toDefHandler :: Sexp.T -> Maybe DefHandler
toDefHandler form
  | isDefHandler form =
    case form of
      _nameDefHandler Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        DefHandler sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDefHandler :: DefHandler -> Sexp.T
fromDefHandler (DefHandler sexp1 sexp2) =
  Sexp.list [Sexp.atom nameDefHandler, sexp1, sexp2]

instance Sexp.Serialize DefHandler where
  deserialize = toDefHandler
  serialize = fromDefHandler

----------------------------------------
-- LetRet
----------------------------------------

nameLetRet :: NameSymbol.T
nameLetRet = ":defret"

isLetRet :: Sexp.T -> Bool
isLetRet (Sexp.Cons form _) = Sexp.isAtomNamed form nameLetRet
isLetRet _ = False

toLetRet :: Sexp.T -> Maybe LetRet
toLetRet form
  | isLetRet form =
    case form of
      _nameLetRet Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        LetRet sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLetRet :: LetRet -> Sexp.T
fromLetRet (LetRet sexp1 sexp2) =
  Sexp.list [Sexp.atom nameLetRet, sexp1, sexp2]

instance Sexp.Serialize LetRet where
  deserialize = toLetRet
  serialize = fromLetRet

----------------------------------------
-- LetOp
----------------------------------------

nameLetOp :: NameSymbol.T
nameLetOp = ":defop"

isLetOp :: Sexp.T -> Bool
isLetOp (Sexp.Cons form _) = Sexp.isAtomNamed form nameLetOp
isLetOp _ = False

toLetOp :: Sexp.T -> Maybe LetOp
toLetOp form
  | isLetOp form =
    case form of
      _nameLetOp Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> Sexp.Nil ->
        LetOp sexp1 sexp2 sexp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLetOp :: LetOp -> Sexp.T
fromLetOp (LetOp sexp1 sexp2 sexp3) =
  Sexp.list [Sexp.atom nameLetOp, sexp1, sexp2, sexp3]

instance Sexp.Serialize LetOp where
  deserialize = toLetOp
  serialize = fromLetOp

----------------------------------------
-- RecordDec
----------------------------------------

nameRecordDec :: NameSymbol.T
nameRecordDec = ":record-d"

isRecordDec :: Sexp.T -> Bool
isRecordDec (Sexp.Cons form _) = Sexp.isAtomNamed form nameRecordDec
isRecordDec _ = False

toRecordDec :: Sexp.T -> Maybe RecordDec
toRecordDec form
  | isRecordDec form =
    case form of
      _nameRecordDec Sexp.:> nameUsage1
        | Just nameUsage1 <- toNameUsage `fromStarList` nameUsage1 ->
          RecordDec nameUsage1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromRecordDec :: RecordDec -> Sexp.T
fromRecordDec (RecordDec nameUsage1) =
  Sexp.listStar [Sexp.atom nameRecordDec, fromNameUsage `toStarList` nameUsage1]

instance Sexp.Serialize RecordDec where
  deserialize = toRecordDec
  serialize = fromRecordDec

----------------------------------------
-- Primitive
----------------------------------------

namePrimitive :: NameSymbol.T
namePrimitive = ":primitive"

isPrimitive :: Sexp.T -> Bool
isPrimitive (Sexp.Cons form _) = Sexp.isAtomNamed form namePrimitive
isPrimitive _ = False

toPrimitive :: Sexp.T -> Maybe Primitive
toPrimitive form
  | isPrimitive form =
    case form of
      _namePrimitive Sexp.:> sexp1 Sexp.:> Sexp.Nil ->
        Primitive sexp1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromPrimitive :: Primitive -> Sexp.T
fromPrimitive (Primitive sexp1) =
  Sexp.list [Sexp.atom namePrimitive, sexp1]

instance Sexp.Serialize Primitive where
  deserialize = toPrimitive
  serialize = fromPrimitive

----------------------------------------
-- Binder
----------------------------------------

nameBinder :: NameSymbol.T
nameBinder = ":<-"

isBinder :: Sexp.T -> Bool
isBinder (Sexp.Cons form _) = Sexp.isAtomNamed form nameBinder
isBinder _ = False

toBinder :: Sexp.T -> Maybe Binder
toBinder form
  | isBinder form =
    case form of
      _nameBinder Sexp.:> nameSymbol1 Sexp.:> sexp2 Sexp.:> Sexp.Nil
        | Just nameSymbol1 <- toNameSymbol nameSymbol1 ->
          Binder nameSymbol1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromBinder :: Binder -> Sexp.T
fromBinder (Binder nameSymbol1 sexp2) =
  Sexp.list [Sexp.atom nameBinder, fromNameSymbol nameSymbol1, sexp2]

instance Sexp.Serialize Binder where
  deserialize = toBinder
  serialize = fromBinder

----------------------------------------
-- DoDeep
----------------------------------------

nameDoDeep :: NameSymbol.T
nameDoDeep = ":do"

isDoDeep :: Sexp.T -> Bool
isDoDeep (Sexp.Cons form _) = Sexp.isAtomNamed form nameDoDeep
isDoDeep _ = False

toDoDeep :: Sexp.T -> Maybe DoDeep
toDoDeep form
  | isDoDeep form =
    case form of
      _nameDoDeep Sexp.:> doBodyFull1
        | Just doBodyFull1 <- toDoBodyFull `fromStarList` doBodyFull1 ->
          DoDeep doBodyFull1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDoDeep :: DoDeep -> Sexp.T
fromDoDeep (DoDeep doBodyFull1) =
  Sexp.listStar [Sexp.atom nameDoDeep, fromDoBodyFull `toStarList` doBodyFull1]

instance Sexp.Serialize DoDeep where
  deserialize = toDoDeep
  serialize = fromDoDeep

----------------------------------------
-- DoPure
----------------------------------------

nameDoPure :: NameSymbol.T
nameDoPure = ":do-pure"

isDoPure :: Sexp.T -> Bool
isDoPure (Sexp.Cons form _) = Sexp.isAtomNamed form nameDoPure
isDoPure _ = False

toDoPure :: Sexp.T -> Maybe DoPure
toDoPure form
  | isDoPure form =
    case form of
      _nameDoPure Sexp.:> sexp1 Sexp.:> Sexp.Nil ->
        DoPure sexp1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDoPure :: DoPure -> Sexp.T
fromDoPure (DoPure sexp1) =
  Sexp.list [Sexp.atom nameDoPure, sexp1]

instance Sexp.Serialize DoPure where
  deserialize = toDoPure
  serialize = fromDoPure

----------------------------------------
-- DoOp
----------------------------------------

nameDoOp :: NameSymbol.T
nameDoOp = ":do-op"

isDoOp :: Sexp.T -> Bool
isDoOp (Sexp.Cons form _) = Sexp.isAtomNamed form nameDoOp
isDoOp _ = False

toDoOp :: Sexp.T -> Maybe DoOp
toDoOp form
  | isDoOp form =
    case form of
      _nameDoOp Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        DoOp sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDoOp :: DoOp -> Sexp.T
fromDoOp (DoOp sexp1 sexp2) =
  Sexp.list [Sexp.atom nameDoOp, sexp1, sexp2]

instance Sexp.Serialize DoOp where
  deserialize = toDoOp
  serialize = fromDoOp

----------------------------------------
-- Via
----------------------------------------

nameVia :: NameSymbol.T
nameVia = ":via"

isVia :: Sexp.T -> Bool
isVia (Sexp.Cons form _) = Sexp.isAtomNamed form nameVia
isVia _ = False

toVia :: Sexp.T -> Maybe Via
toVia form
  | isVia form =
    case form of
      _nameVia Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        Via sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromVia :: Via -> Sexp.T
fromVia (Via sexp1 sexp2) =
  Sexp.list [Sexp.atom nameVia, sexp1, sexp2]

instance Sexp.Serialize Via where
  deserialize = toVia
  serialize = fromVia

----------------------------------------
-- Header
----------------------------------------

nameHeader :: NameSymbol.T
nameHeader = ":header"

isHeader :: Sexp.T -> Bool
isHeader (Sexp.Cons form _) = Sexp.isAtomNamed form nameHeader
isHeader _ = False

toHeader :: Sexp.T -> Maybe Header
toHeader form
  | isHeader form =
    case form of
      _nameHeader Sexp.:> nameSymbol1 Sexp.:> sexp2
        | Just nameSymbol1 <- toNameSymbol nameSymbol1 ->
          Header nameSymbol1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromHeader :: Header -> Sexp.T
fromHeader (Header nameSymbol1 sexp2) =
  Sexp.listStar [Sexp.atom nameHeader, fromNameSymbol nameSymbol1, sexp2]

instance Sexp.Serialize Header where
  deserialize = toHeader
  serialize = fromHeader
