{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Control.Monad.State.Class (MonadState, get, modify)
import Data.Functor.Identity (Identity)
import Data.Map.Strict hiding (foldr)
import Data.Monoid

type Id = String

data Binop = Plus | Minus | Times | Div deriving Show

data Stm = Compound Stm Stm
         | Assign Id Exp
         | Print [Exp]
         deriving Show

data Exp = IdExp Id
         | NumExp Int
         | OpExp Exp Binop Exp
         | EseqExp Stm Exp
         deriving Show

prog :: Stm
prog = Compound (Assign "a" (OpExp (NumExp 5) Plus (NumExp 3)))
                (Compound (Assign "b" (EseqExp (Print [IdExp "a", OpExp (IdExp "a") Minus (NumExp 1)])
                                               (OpExp (NumExp 10) Times (IdExp "a"))))
                          (Print [IdExp "b"]))

{---------------------------------------------------------}

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


class Monad m => MonadPrint m where
  puts :: String -> m ()
  put :: Show a => a -> m ()
  put = puts . show

instance MonadPrint IO where
  puts = putStrLn

instance MonadPrint m => MonadPrint (StateT s m) where
  puts = lift . puts

instance MonadPrint Identity where
  puts _ = return ()

{---------------------------------------------------------}

interpret :: Stm -> IO ()
interpret stm = evalStateT (interpStm stm) empty

type Env = Map Id Int
type MonadEval m = (MonadState Env m, MonadPrint m)

interpStm :: MonadEval m => Stm -> m ()
interpStm (Compound s1 s2) = interpStm s1 >> interpStm s2
interpStm (Assign id exp)  = interpExp exp >>= modify . insert id
interpStm (Print xs)       = traverse interpExp xs >>= putInts

interpExp :: MonadEval m => Exp -> m Int
interpExp (IdExp var)     = (! var) <$> get
interpExp (NumExp x)      = return x
interpExp (EseqExp s exp) = interpStm s >> interpExp exp
interpExp (OpExp e1 b e2) = do
  eval1 <- interpExp e1
  eval2 <- interpExp e2
  return (binopToFn b eval1 eval2)


putInts :: (Show a, MonadPrint m) => [a] -> m ()
putInts = puts . unwords . fmap show
