{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Sexp.Structure.Lens where

import qualified Control.Lens as Lens hiding ((|>))
import qualified Data.Sexp.Structure.Berlin as Berlin
import qualified Data.Sexp.Structure.CoreNamed as Named
import qualified Data.Sexp.Structure.Parsing as Parsing
import qualified Data.Sexp.Structure.Transition as Transition

Lens.makeLensesWith Lens.camelCaseFields ''Transition.DefunMatch
Lens.makeLensesWith Lens.camelCaseFields ''Transition.ArgBody
Lens.makeLensesWith Lens.camelCaseFields ''Transition.LambdaCase
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.Signature
Lens.makeLensesWith Lens.camelCaseFields ''Transition.DefunSigMatch
Lens.makeLensesWith Lens.camelCaseFields ''Transition.RecordNoPunned
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.Defun
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.Let
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.Alias
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.Include
Lens.makeLensesWith Lens.camelCaseFields ''Transition.LetMatch
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.Cond
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.PredAns
Lens.makeLensesWith Lens.camelCaseFields ''Transition.If
Lens.makeLensesWith Lens.camelCaseFields ''Transition.IfNoElse
Lens.makeLensesWith Lens.camelCaseFields ''Transition.SumCon
Lens.makeLensesWith Lens.camelCaseFields ''Transition.SumConFilled
Lens.makeLensesWith Lens.camelCaseFields ''Berlin.Relocated
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.Case
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.DeconBody
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.Arrow
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.Lambda
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.Record
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.RecordDec
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.NameUsage
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.Punned
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.NotPunned
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.Infix
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.OpenIn
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.Open
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.Declare
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.Declaim
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.Type
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.LetType
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.LetSignature
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.DefModule
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.LetModule
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.Header
Lens.makeLensesWith Lens.camelCaseFields ''Named.Star
Lens.makeLensesWith Lens.camelCaseFields ''Named.PrimTy
Lens.makeLensesWith Lens.camelCaseFields ''Named.Prim
Lens.makeLensesWith Lens.camelCaseFields ''Named.Pi
Lens.makeLensesWith Lens.camelCaseFields ''Named.Binder
Lens.makeLensesWith Lens.camelCaseFields ''Named.Lam
Lens.makeLensesWith Lens.camelCaseFields ''Named.Sigma
Lens.makeLensesWith Lens.camelCaseFields ''Named.Pair
Lens.makeLensesWith Lens.camelCaseFields ''Named.Let
Lens.makeLensesWith Lens.camelCaseFields ''Named.Var
Lens.makeLensesWith Lens.camelCaseFields ''Named.App
Lens.makeLensesWith Lens.camelCaseFields ''Named.Ann
Lens.makeLensesWith Lens.camelCaseFields ''Named.Meta
Lens.makeLensesWith Lens.camelCaseFields ''Named.Dot
Lens.makeLensesWith Lens.camelCaseFields ''Named.Field
Lens.makeLensesWith Lens.camelCaseFields ''Named.RecordTy
Lens.makeLensesWith Lens.camelCaseFields ''Named.CatProduct
Lens.makeLensesWith Lens.camelCaseFields ''Named.CatProductIntro
Lens.makeLensesWith Lens.camelCaseFields ''Named.CatProductElimLeft
Lens.makeLensesWith Lens.camelCaseFields ''Named.CatProductElimRight
Lens.makeLensesWith Lens.camelCaseFields ''Named.CatCoproductIntroLeft
Lens.makeLensesWith Lens.camelCaseFields ''Named.CatCoproductIntroRight
Lens.makeLensesWith Lens.camelCaseFields ''Named.CatCoproductElim
Lens.makeLensesWith Lens.camelCaseFields ''Named.CatCoProduct
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.DefHandler
Lens.makeLensesWith Lens.camelCaseFields ''Transition.LetHandler
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.Effect
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.LetRet
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.LetOp
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.Do
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.DoOp
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.DoPure
Lens.makeLensesWith Lens.camelCaseFields ''Parsing.Primitive
