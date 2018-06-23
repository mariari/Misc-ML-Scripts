module InterpretType where

type Id = String

data Binop = Plus | Minus | Times | Div deriving (Show,Eq, Ord)

data Stm = Compound Stm Stm
         | Assign Id Exp
         | Print [Exp]
         deriving Show

data Exp = IdExp Id
         | NumExp Int
         | OpExp Exp Binop Exp
         | EseqExp Stm Exp
         deriving Show
