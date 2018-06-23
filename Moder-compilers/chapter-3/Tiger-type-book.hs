

type Line = Int
type Column = Int
type Symbol = String

type Pos = (Line, Column)

data Var = SimpleVar Symbol Pos
         | FieldVar Var Symbol Pos
         | SubscriptVar Var Exp Pos
         deriving (Show)

data Exp = VarExp Var
         | NilExp
         | IntExp Int
         | StringExp Int Pos
         | CallExp { func :: Symbol
                   , args :: [Exp]
                   , pos  :: Pos
                   }
         | OpExp { left :: Exp
                 , op :: Op
                 , right :: Exp
                 }
         | RecordExp { fields :: [(Symbol, Exp, Pos)]
                     , typ    :: Symbol
                     , pos    :: Pos
                     }
         | SeqExp [(Exp, Pos)]
         | AssignExp { var :: Var
                     , exp :: Exp
                     , pos :: Pos
                     }
         | IfExp { test  :: Exp
                 , then' :: Exp
                 , else' :: Maybe Exp
                 , pos   :: Pos
                 }
         | WhileExp { test :: Exp
                    , body :: Exp
                    , pos  :: Pos
                    }
         | ForExp { var    :: Var
                  , escape :: Bool -- supposed to be a ref...
                  , lo     :: Exp
                  , hi     :: Exp
                  , body   :: Exp
                  , pos    :: Pos
                  }
         | BreakExp Pos
         | LetExp { decs :: [Dec]
                   , body :: Exp
                   , pos  :: Pos
                   }
         | ArrayExp { typ  :: Symbol
                    , size :: Exp
                    , init :: Exp
                    , pos  :: Pos
                    }
         deriving (Show)

data Dec = FunctionDec [FunDec]
         | VarDec { name    :: Symbol
                  , escapeD :: Bool -- supposed to be ref
                  , typD    :: Maybe (Symbol, Pos)
                  , initD   :: Exp
                  , posD    :: Pos
                  }
         | TypeDec { name :: Symbol
                   , ty   :: Ty
                   , posD :: Pos
                   }
         deriving (Show)

data Ty = NameTy Symbol Pos
        | RecordTy [Field]
        | ArrayTy Symbol Pos
        deriving (Show)

data Op = Plus | Minus | Times | Div | Eq | Neq | Lt | Le | Gt | Ge deriving (Show)

type Field  = (Symbol, Bool, Symbol, Pos) -- Bool should be a ref!
data FunDec = FunDec Symbol [Field] (Maybe (Symbol,Pos)) Exp Pos deriving (Show)


data RefMap = RefMap {esc :: Bool} -- use in the state monad
