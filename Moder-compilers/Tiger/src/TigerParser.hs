module TigerParser where

import Text.Parsec
import Text.Parsec.String
import Data.Monoid
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T
import Control.Monad.Identity
import Text.Parsec.Expr as E

import TigerType

langaugeDef :: GenLanguageDef String u Identity
langaugeDef = emptyDef { T.reservedNames   = ["array", "if", "then", "else"
                                             ,"while", "for", "to", "do", "let"
                                             ,"in", "function", "var", "type"
                                             ,"import", "primitive"]
                       , T.reservedOpNames = [",", ":", ";", "(", ")", "[", "]",
                                              "{", "}", ".", "+", "-", "*", "/",
                                              "=", "<>", "<", "<=", ">", ">=", "&", "|", ":="]
                       , T.identStart      = letter <|> char '_'
                       , T.identLetter     = alphaNum <|> char '_'
                       , T.caseSensitive   = True
                       , nestedComments    = True
                       , commentStart      = "/*"
                       , commentEnd        = "*/"}


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
braces     = T.braces     lexer
brackets   = T.brackets   lexer
natural    = T.natural    lexer


sourceLineCol source = (sourceLine source, sourceColumn source)

getLineCol = fmap sourceLineCol getPosition

symbol = identifier

parseTiger = expression

expression :: Parser Exp
expression = buildExpressionParser optable expression' <?> "Exp"

expression' :: Parser Exp
expression' =  seq'
           <|> (ifThen <?> "if then")
           <|> for
           <|> while
           <|> negation
           <|> (let' <?> "let expression")
           <|> break'
           <|> nil
           <|> try arrCreate
           <|> try recCreate
           <|> try funcall
           <|> try assign
           <|> try lvalue
           <|> stringLit
           <|> intLit

dec :: Parser Dec
dec =  tydec
   <|> vardec
   <|> fundec
   <?> "declaration"

tyP :: Parser Ty
tyP =  arrty
   <|> recty
   <|> namety
   <?> "type creation"

-- Exp------------------------------------------------------

seq' = do
  pos <- getLineCol
  seq <- parens (expression `sepBy` semi)
  return $ Sequence seq pos

ifThen :: Parser Exp
ifThen = do
  pos <- getLineCol
  reserved "if"
  pred <- expression
  reserved "then"
  then' <- expression
  optional (reserved "else")
  else' <- optionMaybe expression
  case else' of
    Just x  -> return $ IfThenElse pred then' x pos
    Nothing -> return $ IfThen     pred then'   pos

for :: Parser Exp
for = do
  pos <- getLineCol
  reserved "for"
  var <- identifier
  reservedOp ":="
  from <- expression
  reserved "to"
  end <- expression
  reserved "do"
  run <- expression
  return $ For var from end run pos

while = do
  pos <- getLineCol
  reserved "while"
  pred <- expression
  reserved "do"
  run <- expression
  return $ While pred run pos

assign = do
  pos <- getLineCol
  lvalue <- lvalueParser
  reservedOp ":="
  exp <- expression
  return $ Assign lvalue exp pos

negation = do
  pos <- getLineCol
  reservedOp "-"
  exp <- expression
  return $ Negation exp pos

let' = do
  pos <- getLineCol
  reserved "let"
  decs <- many1 dec
  reserved "in"
  exps <- expression `sepBy` semi -- 0 or more, so no sepBy1
  reserved "end"
  return $ Let decs exps pos

funcall = do
  pos <- getLineCol
  id  <- identifier
  exps <- parens (expression `sepBy` comma)
  return $ Funcall id exps pos

arrCreate = do
  pos <- getLineCol
  tyid <- symbol
  exp  <- brackets expression
  reserved "of"
  exp2 <- expression
  return $ ArrCreate tyid exp exp2 pos

recCreate = do
  pos <- getLineCol
  tyid   <- symbol
  fields <- braces (field' `sepBy` comma)
  return $ RecCreate tyid fields pos

field' :: Parser Field
field' = do
  pos <- getLineCol
  id' <- identifier
  reservedOp "="
  exp <- expression
  return $ Field id' exp pos

break' = (getLineCol >>= return . Break) <* reserved "break"

intLit = do
  pos <- getLineCol
  int <- integer
  return $ IntLit int pos


-- Haskells string has the same escape characters I think!

stringLit :: Parser Exp
stringLit = do
  pos <- getLineCol
  char '"'
  string <- many (noneOf "\"")
  char '"'
  spaces
  return $ StringLit string pos

nil = (getLineCol >>= return . Nil) <* reserved "nil"

-- Lvalue---------------------------------------------------------------------------
-- The following section for lvalue is modified from stackoverflow
leftRec :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (a -> a) -> ParsecT s u m a
leftRec p op = p >>= rest
  where
    rest x = (op >>= rest . ($ x)) <|> return x

lvalue = Var <$> lvalueParser

lvalueParser :: Parser Var
lvalueParser = leftRec idParser (fieldExp <|> subscript)
  where
    idParser = do
      pos <- getLineCol
      id' <- identifier
      return $ SimpleVar id' pos

fieldExp :: ParsecT String u Identity (Var -> Var)
fieldExp = do
  pos <- getLineCol
  reservedOp "."
  a <- identifier
  return $ (\l -> FieldVar l a pos)

subscript :: ParsecT String () Identity (Var -> Var)
subscript = do
  pos <- getLineCol
  e <- brackets expression
  return (\l -> Subscript l e pos)

-- Typ----------------------------------------------------------------------
arrty :: Parser Ty
arrty = do
  pos <- getLineCol
  reserved "array"
  reserved "of"
  sym <- symbol
  return $ ArrayTy sym pos

recty = RecordTy <$> braces (fieldDec `sepBy` comma)

namety = do
  pos <- getLineCol
  id' <- identifier
  return $ NameTy id' pos

-- Declarations--------------------------------------------------------------
fieldDec :: Parser FieldDec
fieldDec = do
  pos <- getLineCol
  id' <- identifier
  reservedOp ":"
  typid <- symbol
  return $ FieldDec id' typid pos

tydec :: Parser Dec
tydec = do
  pos <- getLineCol
  reserved "type"
  typid <- symbol
  reservedOp "="
  ty <- tyP
  return $ TypeDec typid ty pos

fundec :: Parser Dec
fundec = do
  pos <- getLineCol
  reserved "function"
  id'    <- identifier
  fields <- parens (fieldDec `sepBy` comma)
  optional (reservedOp ":")
  mtypid <- optionMaybe identifier
  reservedOp "="
  exp <- expression
  return $ FunDec id' fields mtypid exp pos

vardec :: Parser Dec
vardec = do
  pos <- getLineCol
  reserved "var"
  id' <- identifier
  optional (reservedOp ":")
  mtypid <- optionMaybe identifier
  reservedOp ":="
  exp <- expression
  return $ VarDec id' mtypid exp pos

-- Exp parser for numbers

infixOp op pos exp1 exp2 = Infix' exp1 op exp2 pos

createInfix :: String -> Op -> Parser (Exp -> Exp -> Exp)
createInfix opStr term = (getLineCol >>= return . infixOp term) <* reservedOp opStr

listToChoice :: [(String, Op)] -> Parser (Exp -> Exp -> Exp)
listToChoice = choice . fmap (uncurry createInfix)

createOpTable term = Infix term AssocLeft

timesDiv    = listToChoice [("*", Times), ("/", Div)]
addMinus    = listToChoice [("+",  Plus), ("-", Minus)]
comparisons = listToChoice [("=", Eq), ("<>", Neq), ("<", Lt), ("<=", Le), (">", Gt), (">=", Ge)]

optable = [[createOpTable timesDiv]
          ,[createOpTable addMinus]
          ,[createOpTable comparisons]]
