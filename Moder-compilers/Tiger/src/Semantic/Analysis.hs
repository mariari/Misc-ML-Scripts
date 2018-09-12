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

transExp :: Env.TypeMap -> Env.EnvMap -> Absyn.Exp -> Expty
transExp tm em absyn = case runMonadTranS tm em (transExp' False absyn) of
  Left a          -> error a
  Right (expt,tl) -> expt

transExp' :: MonadTranS m => Bool -> Absyn.Exp -> m Expty
transExp' _ (Absyn.IntLit _ _)    = return (Expty {expr = (), typ = PT.INT})
transExp' _ (Absyn.Nil _)         = return (Expty {expr = (), typ = PT.NIL})
transExp' _ (Absyn.StringLit _ _) = return (Expty {expr = (), typ = PT.STRING})
transExp' insideLoop (Absyn.Infix' left x right pos) = case x of
  Absyn.Minus -> handleInfixInt insideLoop left right pos
  Absyn.Plus  -> handleInfixInt insideLoop left right pos
  Absyn.Times -> handleInfixInt insideLoop left right pos
  Absyn.Div   -> handleInfixInt insideLoop left right pos
  Absyn.And   -> handleInfixInt insideLoop left right pos
  Absyn.Or    -> handleInfixInt insideLoop left right pos
  Absyn.Gt    -> handleInfixStrInt insideLoop left right pos
  Absyn.Ge    -> handleInfixStrInt insideLoop left right pos
  Absyn.Lt    -> handleInfixStrInt insideLoop left right pos
  Absyn.Le    -> handleInfixStrInt insideLoop left right pos
  Absyn.Eq    -> handleInfixSame insideLoop left right pos
  Absyn.Neq   -> handleInfixSame insideLoop left right pos
transExp' insideLoop (Absyn.Break pos)
  | insideLoop = return (Expty {expr = (), typ = PT.NIL})
  | otherwise  = throwError (show pos <> " break needs to be used inside a loop")
transExp' insideLoop (Absyn.Let decs exps pos) = do
  traverse transDec decs
  case exps of
    []   -> return (Expty {expr = (), typ = PT.NIL})
    exps -> last <$> (traverse (transExp' insideLoop) exps)
transExp' _ (Absyn.Var x) = do
  env  <- get
  runReaderT (transVar x) env
transExp' insideLoop (Absyn.While pred body pos) = do
  pred' <- transExp' insideLoop pred
  body' <- transExp' True body
  checkInt pred' pos
  checkNil body' pos
  return (Expty {expr = (), typ = PT.NIL})
transExp' insideLoop (Absyn.For var from to body pos) = do
  from' <- transExp' insideLoop from
  to'   <- transExp' insideLoop to
  Trans {tm = tm, em = em} <- get
  checkInt from' pos
  checkInt to' pos
  body' <- locallyInsert1 (transExp' True body)
                          (var, Env.VarEntry {Env.ty = PT.INT, Env.modifiable = False})
  checkNil body' pos -- the false makes it so if we try to modify it, it errors
  return body'

-- transVar doesn't go to Dec, so it's a reader
transVar :: MonadTranR m => Absyn.Var -> m Expty
transVar = undefined

transDec :: MonadTranS m => Absyn.Dec -> m ()
transDec = undefined

transTy :: Env.EnvMap -> Absyn.Exp -> PT.Type
transTy = undefined

-- Helper functions----------------------------------------------------------------------------
-- adds a symbol to the envEntry replacing what is there for this scope
locallyInsert1 :: MonadTranS m => m b -> (S.Symbol, Env.EnvEntry) -> m b
locallyInsert1 expression (symb, envEntry) = do
  Trans {tm = typeMap, em = envMap} <- get
  let val = envMap Map.!? symb
  changeEnvValue symb (Just envEntry)

  expResult <- expression

  changeEnvValue symb val

  return expResult

-- adds symbols to the envEntry replacing what is there for this scope
locallyInsert :: MonadTranS m => m b -> [(S.Symbol, Env.EnvEntry)] -> m b
locallyInsert expression xs = do
  Trans {tm = typeMap, em = envMap} <- get
  let vals = fmap (\(symb,_) -> (symb, envMap Map.!? symb)) xs

  traverse (\(symb, envEntry) -> changeEnvValue symb (Just envEntry)) xs

  expResult <- expression

  traverse (uncurry changeEnvValue) vals
  return expResult

-- Changes the Envrionment value... removing a value if there is none, else places the new value in the map
changeEnvValue :: MonadState Translation m => S.Symbol -> Maybe Env.EnvEntry -> m ()
changeEnvValue symb val = do
  Trans {tm = typeMap, em = envMap} <- get
  case val of
    Nothing   -> put (Trans {tm = typeMap, em = Map.delete symb envMap})
    Just val' -> put (Trans {tm = typeMap, em = Map.insert symb val' envMap})

-- this function will eventually become deprecated once we handle the intermediate stage
handleInfixExp :: MonadTranS m
               => (Expty -> Absyn.Pos -> m ()) -- A function like checkInt
               -> Bool                         -- whether we are inside a loop or not
               -> Absyn.Exp                    -- left side of infix
               -> Absyn.Exp                    -- right side of infix
               -> Absyn.Pos                    -- the Posiiton
               -> m Expty
handleInfixExp f insideLoop left right pos = do
  left'  <- transExp' insideLoop left
  right' <- transExp' insideLoop right
  f left' pos
  f right' pos
  return (Expty {expr = (), typ = PT.INT})

-- will become deprecated once we handle the intermediate stage
handleInfixSame :: MonadTranS m => Bool -> Absyn.Exp -> Absyn.Exp -> Absyn.Pos -> m Expty
handleInfixSame insideLoop left right pos = do
  left'  <- transExp' insideLoop left
  right' <- transExp' insideLoop right
  checkSame left' right' pos
  return (Expty {expr = (), typ = PT.INT})

handleInfixInt, handleInfixStrInt, handleInfixStr
  :: MonadTranS m => Bool -> Absyn.Exp -> Absyn.Exp -> Absyn.Pos -> m Expty
handleInfixInt    = handleInfixExp checkInt
handleInfixStr    = handleInfixExp checkStr
handleInfixStrInt = handleInfixExp checkStrInt


-- Check checks whether the arguments are of the correct type if not throw a monadError
checkInt :: (MonadError String m, Show a) => Expty -> a -> m ()
checkInt (Expty {typ = PT.INT}) pos = return  ()
checkInt (Expty {typ = _})      pos = throwError (show pos <> " integer required")

checkNil :: (MonadError String m, Show a) => Expty -> a -> m ()
checkNil (Expty {typ = PT.NIL}) pos = return ()
checkNil (Expty {typ = _})      pos = throwError (show pos <> " null required")

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
