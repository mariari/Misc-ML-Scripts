
import Text.Parsec
import Text.Parsec.String
import Data.Monoid
import Text.ParserCombinators.Parsec.Language
import qualified Text.Parsec.Token as T
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
symbol     = T.symbol     lexer


myIdent = many1 letter >>= \x -> skipMany space >> return x
test    = runParser ( (myIdent <|> parens myIdent) `sepBy` semi) () "" "(etstefasasdfsa) ; wew ; gay ; wow ; wew"