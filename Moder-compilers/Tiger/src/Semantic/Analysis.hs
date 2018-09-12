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


type MonadTranErr m = MonadError String m
type MonadTranS   m = (MonadState  Translation m, MonadTranErr m)
type MonadTranR   m = (MonadReader Translation m, MonadTranErr m)
type MonadTranSIO m = (MonadIO m, MonadTranS m)

runMonadTranS :: Env.TypeMap
              -> Env.EnvMap
              -> StateT Translation (Except String) a
              -> Either String (a, Translation)
runMonadTranS tm em f = runExcept (runStateT f trans)
  where trans = Trans {tm = tm, em = em}

-- transVar doesn't go to Dec, so it's a reader
transVar :: MonadTranR m => Absyn.Var -> m Expty
transVar = undefined

transExp :: MonadTranS m => Absyn.Exp -> m Expty
tranExp (Absyn.Infix' left Absyn.Plus right pos) = do
  left  <- transExp left
  right <- transExp right
  lift (checkInt left pos)
  lift (checkInt right pos)
  return (Expty {expr = (), typ = PT.INT})
transExp x = undefined

transDec :: MonadTranS m => Absyn.Exp -> m ()
transDec = undefined

transTy :: Env.EnvMap -> Absyn.Exp -> PT.Type
transTy = undefined


-- Helper functions----------------------------------------------------------------------------
checkInt :: (MonadError String m, Show a) => Expty -> a -> m ()
checkInt (Expty {typ = PT.INT}) pos = return  ()
checkInt (Expty {typ = _})      pos = throwError (show pos <> " integer required")
