{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Semantic.Analysis where

import qualified ProgramTypes         as PT
import qualified TigerType            as Absyn
import qualified Semantic.Environment as Env

import           Data.Unique
import qualified Data.Symbol as S
import qualified Data.Map.Strict as Map -- we are use ordering in symbols, so we can't use HashMap
import           Control.Monad.State.Lazy
import           Control.Monad.Reader

-- Translation type
-- will be expanded upon in chapter 7
type TranslateExp = ()

data Expty = Expty { exp :: TranslateExp
                   , typ :: PT.Type
                   } deriving Show

data Translation = Trans { tm :: Env.TypeMap
                         , em :: Env.EnvMap
                         } deriving Show

type MonadTranslationS   m = MonadState  Translation m
type MonadTranslationR   m = MonadReader Translation m
type MonadTranlsationSIO m = (MonadIO m, MonadTranslationS m)


-- transVar doesn't go to Dec, so it's a reader
transVar :: MonadTranslationR m => Absyn.Var -> m Expty
transVar = undefined

transExp :: MonadTranslationS m => Absyn.Exp -> m Expty
transExp = undefined

transDec :: MonadTranslationS m => Absyn.Exp -> m ()
transDec = undefined

transTy :: Env.EnvMap -> Absyn.Exp -> PT.Type
transTy = undefined
