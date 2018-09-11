module TypeChecker where

import ProgramTypes

import           Data.Unique
import qualified Data.Symbol as S
import qualified Data.Map.Strict as Map -- we are use ordering in symbols, so we can't use HashMap

-- Environment
type TypeMap = SymMap Type     -- for types
type EnvMap  = SymMap EnvEntry -- for functions and variables

data EnvEntry = VarEntry {ty :: Type}
              | FunEntry {formals :: [Type]
                         ,result  :: Type}
              deriving Show

baseTenv :: TypeMap
baseTenv = Map.fromList [(S.intern "int",    INT)
                        ,(S.intern "string", STRING)
                        ,(S.intern "nil",    NIL)]

baseVenv :: EnvMap
baseVenv = Map.fromList [(S.intern "print",     FunEntry [STRING] UNIT)
                        ,(S.intern "flush",     FunEntry []       UNIT)
                        ,(S.intern "getchar",   FunEntry []       STRING)
                        ,(S.intern "ord",       FunEntry [STRING] INT)
                        ,(S.intern "chr",       FunEntry [INT]    STRING)
                        ,(S.intern "size",      FunEntry [STRING] INT)
                        ,(S.intern "not",       FunEntry [INT]    INT)
                        ,(S.intern "exit",      FunEntry [INT]    UNIT)
                        ,(S.intern "substring", FunEntry [STRING, INT, INT] STRING)
                        ,(S.intern "concat",    FunEntry [STRING, STRING]   STRING)]
