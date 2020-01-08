module Parser where

import Text.Parsec
import Text.Parsec.String
import Data.Monoid
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T
import Control.Monad.Identity

import InterpretType

langaugeDef :: GenLanguageDef String u Identity
langaugeDef = emptyDef { T.reservedNames   = ["print"]
                       , T.reservedOpNames = [":=", "+", "/", "*", "-"]
                       , T.identStart      = letter
                       , T.identLetter     = alphaNum
                       , T.caseSensitive   = True}

lexer :: T.GenTokenParser String u Identity
lexer = T.makeTokenParser langaugeDef

-- could make these
identifier = T.identifier lexer
reserved   = T.reserved   lexer
reservedOp = T.reservedOp lexer
parens     = T.parens     lexer
integer    = T.integer    lexer
semi       = T.semi       lexer
semiSep    = T.semiSep    lexer
whiteSpace = T.whiteSpace lexer
comma      = T.comma      lexer

inlineParser :: Parser Stm
inlineParser = whiteSpace >> statement


statement :: Parser Stm
statement = foldr1 Compound <$> sepBy1 statement' semi

statement' :: Parser Stm
statement' = assignStm
         <|> printStm


expression :: Parser Exp
expression =  try opExp <|> expression'

expression' =  numExp
           <|> idExp
           <|> eseqExp

binop :: Parser Binop
binop =  plus
     <|> minus
     <|> times
     <|> div'

assignStm :: Parser Stm
assignStm = do
  var <- identifier
  reservedOp ":="
  exp <- expression
  return $ Assign var exp

printStm :: Parser Stm
printStm = do
  reserved "print"
  exps <- parens (sepBy expression comma)
  return $ Print exps

idExp :: Parser Exp
idExp = IdExp <$> identifier

numExp :: Parser Exp
numExp = NumExp . fromInteger <$> integer

opExp :: Parser Exp
opExp = do
  exp1  <- expression'
  binop <- binop
  exp2  <- expression
  return $ OpExp exp1 binop exp2

eseqExp :: Parser Exp
eseqExp = parens $ do
  stm <- statement
  comma
  exp <- expression
  return $ EseqExp stm exp

plus :: Parser Binop
plus  = reservedOp "+" >> return Plus
minus = reservedOp "-" >> return Minus
times = reservedOp "*" >> return Times
div'  = reservedOp "/" >> return Div

test parser = runParser parser () "" "a := 5+3; b := (print((a := 12, a), a-1), 10*a); print(b)"
