{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Semantic.Analysis where

import qualified ProgramTypes         as PT
import qualified TigerType            as Absyn
import qualified Semantic.Environment as Env

import           Data.Monoid((<>))
import           Data.Unique
import qualified Data.Symbol as S
import qualified Data.Map.Strict as Map -- we are use ordering in symbols, so we can't use HashMap
import           Control.Monad.State.Lazy
import           Control.Monad.Reader
import           Control.Monad.Except

-- Translation type
-- will be expanded upon in chapter 7
type TranslateExp = ()

data Expty = Expty { expr :: TranslateExp
                   , typ  :: PT.Type
                   } deriving Show

data Translation = Trans { tm :: Env.TypeMap
                         , em :: Env.EnvMap
                         } deriving Show


type MonadTranErr m = (MonadError String m)
type MonadTranS   m = (MonadState  Translation m, MonadTranErr m)
type MonadTranR   m = (MonadReader Translation m, MonadTranErr m)
type MonadTranSIO m = (MonadIO m, MonadTranS m)

runMonadTranS :: Env.TypeMap
              -> Env.EnvMap
              -> StateT Translation (Except String) a
              -> Either String (a, Translation)
runMonadTranS tm em f = runExcept (runStateT f trans)
  where trans = Trans {tm = tm, em = em}

transExp :: MonadTranS m => Absyn.Exp -> m Expty
transExp (Absyn.Infix' left x right pos) = case x of
  Absyn.Minus -> handleInfixInt left right pos
  Absyn.Plus  -> handleInfixInt left right pos
  Absyn.Times -> handleInfixInt left right pos
  Absyn.Div   -> handleInfixInt left right pos
  Absyn.And   -> handleInfixInt left right pos
  Absyn.Or    -> handleInfixInt left right pos
  Absyn.Gt    -> handleInfixStrInt left right pos
  Absyn.Ge    -> handleInfixStrInt left right pos
  Absyn.Lt    -> handleInfixStrInt left right pos
  Absyn.Le    -> handleInfixStrInt left right pos
  Absyn.Eq    -> handleInfixSame left right pos
  Absyn.Neq   -> handleInfixSame left right pos
transExp (Absyn.IntLit _ _) = return (Expty {expr = (), typ = PT.INT})
transExp (Absyn.Nil _)      = return (Expty {expr = (), typ = PT.NIL})
transExp (Absyn.Var x) = do
  env  <- get
  runReaderT (transVar x) env

-- transVar doesn't go to Dec, so it's a reader
transVar :: MonadTranR m => Absyn.Var -> m Expty
transVar = undefined

transDec :: MonadTranS m => Absyn.Exp -> m ()
transDec = undefined

transTy :: Env.EnvMap -> Absyn.Exp -> PT.Type
transTy = undefined

-- Helper functions----------------------------------------------------------------------------

-- this function will eventually become deprecated once we handle the intermediate stage
handleInfixExp :: MonadTranS m
               => (Expty -> Absyn.Pos -> m ()) -- A function like checkInt
               -> Absyn.Exp                    -- left side of infix
               -> Absyn.Exp                    -- right side of infix
               -> Absyn.Pos                    -- the Posiiton
               -> m Expty
handleInfixExp f left right pos = do
  left  <- transExp left
  right <- transExp right
  f left pos
  f right pos
  return (Expty {expr = (), typ = PT.INT})

-- will become deprecated once we handle the intermediate stage
handleInfixSame :: MonadTranS m => Absyn.Exp -> Absyn.Exp -> Absyn.Pos -> m Expty
handleInfixSame left right pos = do
  left  <- transExp left
  right <- transExp right
  checkSame left right pos
  return (Expty {expr = (), typ = PT.INT})

handleInfixInt, handleInfixStrInt, handleInfixStr
  :: MonadTranS m => Absyn.Exp -> Absyn.Exp -> Absyn.Pos -> m Expty
handleInfixInt    = handleInfixExp checkInt
handleInfixStr    = handleInfixExp checkStr
handleInfixStrInt = handleInfixExp checkStrInt


-- Check checks whether the arguments are of the correct type if not throw a monadError
checkInt :: (MonadError String m, Show a) => Expty -> a -> m ()
checkInt (Expty {typ = PT.INT}) pos = return  ()
checkInt (Expty {typ = _})      pos = throwError (show pos <> " integer required")

checkStr :: (MonadError String m, Show a) => Expty -> a -> m ()
checkStr (Expty {typ = PT.STRING}) pos = return  ()
checkStr (Expty {typ = _})         pos = throwError (show pos <> " string required")

checkStrInt :: (MonadError String m, Show a) => Expty -> a -> m ()
checkStrInt (Expty {typ = PT.STRING}) pos = return  ()
checkStrInt (Expty {typ = PT.INT})    pos = return  ()
checkStrInt (Expty {typ = _})         pos = throwError (show pos <> " integer or string required")

checkSame :: (MonadError String m, Show a) => Expty -> Expty -> a -> m ()
checkSame (Expty {typ = x}) (Expty {typ = y}) pos
  | x == y    = return ()
  | otherwise = throwError (show pos <> " given a " <> show x <> " needs to be the same type as " <> show y)
