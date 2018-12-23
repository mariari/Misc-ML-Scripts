{-# LANGUAGE GADTs, StandaloneDeriving, RankNTypes #-}

type Symbol = String
--import Data.IORef.Show

type Program = Exp

type Line = Int
type Column = Int

type Pos = (Line, Column)
type Escape = Bool

-- These are shell types used for the gadt below!
data Void
data Record
data Array

-- This allows me to encode a List of Exp _ thing without worrying about the type
data WrappedExp esc where
  WrapE :: Exp esc p -> WrappedExp esc

deriving instance Show (WrappedExp esc)

unwrapEWith :: WrappedExp esc -> (forall p. Exp esc p -> r) -> r
unwrapEWith (WrapE e) f = f e

data WrappedField esc where
  WrapF :: Field esc p -> WrappedField esc

deriving instance Show (WrappedField esc)

data WrappedDec esc where
  WrapD :: Dec esc p -> WrappedDec esc

deriving instance Show (WrappedDec esc)

data Exp esc p where
  Nil        ::                                          {-# UNPACK #-} !Pos -> Exp esc Void
  Break      ::                                          {-# UNPACK #-} !Pos -> Exp esc Void
  Var        :: Var esc                               -> {-# UNPACK #-} !Pos -> Exp esc Void
  IntLit     :: {-# UNPACK #-} !Int                   -> {-# UNPACK #-} !Pos -> Exp esc Int
  StringLit  :: !String                               -> {-# UNPACK #-} !Pos -> Exp esc String
  Sequence   :: [WrappedExp esc]                      -> {-# UNPACK #-} !Pos -> Exp esc p
  Negation   :: Exp esc Int                           -> {-# UNPACK #-} !Pos -> Exp esc Int
  Infix'     :: Exp esc p -> !Op -> Exp esc p         -> {-# UNPACK #-} !Pos -> Exp esc Int
  ArrCreate  :: !Symbol -> Exp esc Int -> Exp esc p   -> {-# UNPACK #-} !Pos -> Exp esc Array
  RecCreate  :: !Symbol -> [WrappedField esc]         -> {-# UNPACK #-} !Pos -> Exp esc Record
  Funcall    :: !Symbol -> [WrappedExp esc]           -> {-# UNPACK #-} !Pos -> Exp esc p
  Assign     :: Var esc -> Exp esc p                  -> {-# UNPACK #-} !Pos -> Exp esc p
  IfThenElse :: Exp esc Int -> Exp esc p -> Exp esc p -> {-# UNPACK #-} !Pos -> Exp esc p
  IfThen     :: Exp esc Int -> Exp esc Void           -> {-# UNPACK #-} !Pos -> Exp esc Void
  While      :: Exp esc Int -> Exp esc Void           -> {-# UNPACK #-} !Pos -> Exp esc Void
  Let        :: [WrappedDec esc] -> [WrappedExp esc]  -> {-# UNPACK #-} !Pos -> Exp esc p
  For        :: Show (esc Escape)
             => Symbol
             -> esc Escape  -> Exp esc Int
             -> Exp esc Int -> Exp esc Void          -> {-# UNPACK #-} !Pos -> Exp esc Void

deriving instance Show (Exp esc p)

data Field esc p = Field { fieldTyp :: !Symbol
                         , expr     :: Exp esc p
                         , pos      :: {-# UNPACK #-} !Pos
                         } deriving Show

data FieldDec esc where
  FieldDec :: Show (esc Escape)
           => !Symbol
           -> !(esc Escape)
           -> !Symbol
           -> {-# UNPACK #-} !Pos
           -> FieldDec esc
deriving instance Show (FieldDec esc)


data Ty esc = NameTy   !Symbol {-# UNPACK #-} !Pos
            | ArrayTy  !Symbol {-# UNPACK #-} !Pos
            | RecordTy [FieldDec esc]
            deriving Show

-- non-left recursive
data Var esc = SimpleVar !Symbol                 {-# UNPACK #-} !Pos
             | FieldVar  (Var esc) !Symbol       {-# UNPACK #-} !Pos
             | Subscript (Var esc) (Exp esc Int) {-# UNPACK #-} !Pos
             deriving Show

data Dec esc p where
  FunDec :: !Symbol
         -> [FieldDec esc]
         -> Maybe Symbol
         -> Exp esc p
         -> {-# UNPACK #-} !Pos
         -> Dec esc p
  VarDec :: Show (esc Escape)
         => !Symbol
         -> esc Escape
         -> Maybe Symbol
         -> Exp esc p
         -> {-# UNPACK #-} !Pos
         -> Dec esc p
  TypeDec :: !Symbol -> Ty esc -> {-# UNPACK #-} !Pos -> Dec esc p

deriving instance Show (Dec esc p)

data Op = Plus | Minus | Times | Div | Eq | Neq | Lt | Le | Gt | Ge | And | Or deriving (Show,Eq)
