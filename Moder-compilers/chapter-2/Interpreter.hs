module Interpreter where

import Control.Monad.State
import Data.Map.Strict hiding (foldr)
import Data.Monoid

import Parser
import InterpretType

prog :: Stm
prog = Compound (Assign "a" (OpExp (NumExp 5) Plus (NumExp 3)))
                (Compound (Assign "b" (EseqExp (Print [IdExp "a", OpExp (IdExp "a") Minus (NumExp 1)])
                                               (OpExp (NumExp 10) Times (IdExp "a"))))
                          (Print [IdExp "b"]))
-- Gives the maximum number of arguments to a Print statement
maxArgs :: Stm -> Int
maxArgs (Compound s1 s2) = max (maxArgs s1) (maxArgs s2)
maxArgs (Assign _ exp)   = maxArgsExp exp
maxArgs (Print xs)       = maximum (length xs : fmap maxArgsExp xs)

maxArgsExp :: Exp -> Int
maxArgsExp (EseqExp stm _) = maxArgs stm
maxArgsExp other           = 0


binopToFn :: Integral a => Binop -> a -> a -> a
binopToFn Minus = (-)
binopToFn Plus  = (+)
binopToFn Times = (*)
binopToFn Div   = div

interpret :: Stm -> IO ()
interpret stm = evalStateT (interpStm stm) empty

interpStm :: Stm -> StateT (Map Id Int) IO ()
interpStm (Compound s1 s2) = interpStm s1 >> interpStm s2
interpStm (Assign id exp)  = interpExp exp >>= modify . insert id
interpStm (Print xs)       = traverse interpExp xs >>= putShow

interpExp :: Exp -> StateT (Map Id Int) IO Int
interpExp (IdExp var)     = (! var) <$> get
interpExp (NumExp x)      = return x
interpExp (EseqExp s exp) = get >>= (interpStm s *> interpExp exp <*) . put
interpExp (OpExp e1 b e2) = do
  eval1 <- interpExp e1
  eval2 <- interpExp e2
  return (binopToFn b eval1 eval2)

putShow :: (Show a, MonadTrans t) => [a] -> t IO ()
putShow = lift . putStrLn . unwords . fmap show

-- This was from chapter 1 where EseqExpressions carried over
--interpExp (EseqExp s exp) = interpStm s >> interpExp exp
