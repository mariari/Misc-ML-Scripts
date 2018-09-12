module TigerType where

import Data.Symbol

type Program = Exp

type Line = Int
type Column = Int

type Pos = (Line, Column)

data Exp = Var Var
         | Nil                      Pos
         | IntLit Integer                 Pos
         | StringLit String         Pos
         | Break                    Pos
         | Sequence [Exp]           Pos
         | Negation Exp             Pos
         | Funcall Symbol [Exp]     Pos
         | Infix' Exp Op Exp        Pos
         | ArrCreate Symbol Exp Exp Pos
         | RecCreate Symbol [Field] Pos
         | Assign Var Exp           Pos
         | IfThenElse Exp Exp Exp   Pos
         | IfThen     Exp Exp       Pos
         | While Exp Exp            Pos
         | For Symbol Exp Exp Exp   Pos
         | Let [Dec] [Exp]          Pos
         deriving Show

data Field = Field Symbol Exp Pos deriving (Show)

data FieldDec = FieldDec Symbol Symbol Pos deriving (Show)

data Ty = NameTy   Symbol Pos
        | RecordTy [FieldDec]
        | ArrayTy  Symbol Pos
        deriving (Show)

-- non-left recursive
data Var = SimpleVar Symbol Pos
         | FieldVar  Var Symbol Pos
         | Subscript Var Exp Pos
         deriving (Show)


data Dec = FunDec Symbol [FieldDec] (Maybe Symbol) Exp Pos
         | VarDec Symbol (Maybe Symbol) Exp Pos
         | TypeDec Symbol Ty Pos
         deriving (Show)

data Op = Plus | Minus | Times | Div | Eq | Neq | Lt | Le | Gt | Ge | And | Or deriving (Show,Eq)
