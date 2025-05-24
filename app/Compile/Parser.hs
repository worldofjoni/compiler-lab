module Compile.Parser
  ( parseAST,
    parseNumber,
  )
where

import Compile.AST (AST, Expr (..), Op (..), Simp (Asgn, Decl, Init), Stmt (..), Type (BoolType, IntType), UnOp (..))
import Control.Monad.Combinators.Expr
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAlpha, isAscii, isDigit)
import Data.Functor (void)
import Data.Int (Int32)
import Data.Void (Void)
import Error (L1ExceptT, parserFail)
import Numeric (showHex)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

parseAST :: FilePath -> L1ExceptT AST
parseAST path = do
  text <- liftIO $ readFile path
  case parse astParser path text of
    Left err -> parserFail $ errorBundlePretty err
    Right ast -> return ast

parseNumber :: String -> Either String Integer
parseNumber s = do
  case parse number "<literal>" s of
    Left err -> Left $ errorBundlePretty err
    Right n -> Right n

type Parser = Parsec Void String

astParser :: Parser AST
astParser = do
  sc
  -- this parses `int main()` literally, like in the L1 grammar
  reserved "int"
  reserved "main"
  parens $ pure ()
  BlockStmt block _ <- parseBlock
  return block

parseType :: Parser Type
parseType = IntType <$ reserved "int" <|> BoolType <$ reserved "bool" <?> "type"

stmt :: Parser Stmt
stmt =
  SimpStmt
    <$> (simp <* semi)
      <|> parseBlock
      <|> parseIf
      <|> parseWhile
      <|> parseFor
      <|> parseContinue
      <|> parseBreak
      <|> ret

parseBlock :: Parser Stmt
parseBlock = do
  pos <- getSourcePos
  BlockStmt <$> braces (many stmt) <*> return pos

parseIf :: Parser Stmt
parseIf = do
  pos <- getSourcePos
  reserved "if"
  cond <- parens expr
  thenStmt <- stmt
  elseStmt <- optional (reserved "else" >> stmt)
  return $ If cond thenStmt elseStmt pos

parseWhile :: Parser Stmt
parseWhile = do
  pos <- getSourcePos
  reserved "while"
  cond <- parens expr
  body <- stmt
  return $ While cond body pos

parseFor :: Parser Stmt
parseFor = do
  pos <- getSourcePos
  reserved "for"
  (a, b, c) <- parens ((,,) <$> optional simp <* semi <*> expr <* semi <*> optional simp)
  body <- stmt
  return $ For a b c body pos

parseContinue :: Parser Stmt
parseContinue = do
  pos <- getSourcePos
  Contiue <$ reserved "continue" <* semi <*> return pos

parseBreak :: Parser Stmt
parseBreak = do
  pos <- getSourcePos
  Break <$ reserved "break" <* semi <*> return pos

simp :: Parser Simp
simp = asign <|> try declInit <|> try declNoInit

declNoInit :: Parser Simp
declNoInit = do
  pos <- getSourcePos
  type_ <- parseType
  name <- identifier
  return $ Decl type_ name pos

declInit :: Parser Simp
declInit = do
  pos <- getSourcePos
  type_ <- parseType
  name <- identifier
  void $ symbol "="
  e <- expr
  return $ Init type_ name e pos

asign :: Parser Simp
asign = do
  pos <- getSourcePos
  name <- lvalue
  op <- asnOp
  e <- expr
  return $ Asgn name op e pos

asnOp :: Parser (Maybe Op)
asnOp =
  do
    op <- operator
    case op of
      "+=" -> pure (Just Add)
      "*=" -> pure (Just Mul)
      "-=" -> pure (Just Sub)
      "/=" -> pure (Just Div)
      "%=" -> pure (Just Mod)
      "&=" -> pure $ Just BitAnd
      "|=" -> pure $ Just BitOr
      "^=" -> pure $ Just BitXor
      "<<=" -> pure $ Just Shl
      ">>=" -> pure $ Just Shr
      "=" -> pure Nothing
      x -> fail $ "Nonexistent assignment operator: " ++ x
    <?> "assignment operator"

ret :: Parser Stmt
ret = do
  pos <- getSourcePos
  reserved "return"
  e <- expr
  semi
  return $ Ret e pos

expr' :: Parser Expr
expr' = parens expr <|> intExpr <|> boolExpr <|> identExpr

intExpr :: Parser Expr
intExpr = do
  pos <- getSourcePos
  str <- numberLiteral
  return $ IntExpr str pos

boolExpr :: Parser Expr
boolExpr = do
  pos <- getSourcePos
  value <- True <$ reserved "true" <|> False <$ reserved "false"
  return $ BoolExpr value pos

identExpr :: Parser Expr
identExpr = do
  pos <- getSourcePos
  name <- identifier
  return $ IdentExpr name pos

opTable :: [[Operator Parser Expr]]
opTable =
  [ [manyUnaryOp Neg "-", manyUnaryOp Not "!", manyUnaryOp BitNot "~"],
    [infix_ Mul "*", infix_ Div "/", infix_ Mod "%"],
    [infix_ Add "+", infix_ Sub "-"],
    [infix_ Shl "<<", infix_ Shr ">>"],
    [infix_ Lt "<", infix_ Gt ">", infix_ Le "<=", infix_ Ge ">="],
    [infix_ Eq "==", infix_ Neq "!="],
    [infix_ BitAnd "&"],
    [infix_ BitOr "|"],
    [infix_ BitXor "^"],
    [infix_ And "&&"],
    [infix_ Or "||"],
    [TernR (Ternary <$ symbol "?" <$ symbol ":")]
  ]
  where
    -- this allows us to parse `---x` as `-(-(-x))`
    -- makeExprParser doesn't do this by default
    manyUnaryOp op sym = Prefix $ foldr1 (.) <$> some (UnExpr op <$ symbol sym)
    infix_ op sym = InfixL (flip BinExpr op <$ symbol sym)

expr :: Parser Expr
expr = makeExprParser expr' opTable <?> "expression"

-- Lexer starts here, probably worth moving to its own file at some point
sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing (`elem` [' ', '\t', '\r', '\n'])) lineComment blockComment
  where
    lineComment = L.skipLineComment "//"
    blockComment = L.skipBlockCommentNested "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

semi :: Parser ()
semi = void $ symbol ";"

numberLiteral :: Parser String
numberLiteral = lexeme (try hexLiteral <|> decLiteral <?> "number")

-- We want to reject leading zeroes, but `0` itself should of course be accepted
decLiteral :: Parser String
decLiteral = string "0" <|> (:) <$> oneOf ['1' .. '9'] <*> many digitChar

hexLiteral :: Parser String
hexLiteral = do
  void $ char '0'
  void $ char 'x' <|> char 'X'
  digits <- some hexDigitChar
  return ("0x" ++ digits)

number :: Parser Integer
number = try hexadecimal <|> decimal <?> "number"

decimal :: Parser Integer
decimal = do
  n <- lexeme L.decimal
  notFollowedBy alphaNumChar
  if n < maxInt
    then return n
    else
      if n == maxInt
        then return (-maxInt)
        else fail $ "Decimal literal out of bounds: " ++ show n
  where
    maxInt = 2 ^ (31 :: Integer)

hexadecimal :: Parser Integer
hexadecimal = do
  void $ chunk "0x"
  n <- lexeme L.hexadecimal
  if n > maxHex
    then fail $ "Hexadecimal literal out of bounds: " ++ "0x" ++ showHex n ""
    else return $ toInteger ((fromInteger n) :: Int32)
  where
    maxHex = 0xFFFFFFFF

reserved :: String -> Parser ()
reserved w = void $ lexeme $ (string w <* notFollowedBy identLetter)

reservedWords :: [String]
reservedWords =
  [ "alloc",
    "alloc_array",
    "assert",
    "bool",
    "break",
    "char",
    "continue",
    "else",
    "false",
    "for",
    "if",
    "int",
    "NULL",
    "print",
    "read",
    "return",
    "string",
    "struct",
    "true",
    "void",
    "while"
  ]

-- Operations
opStart :: Parser Char
opStart = oneOf "=+-*/%&^|<>!~"

opLetter :: Parser Char
opLetter = oneOf "=&|<>"

operator :: Parser String
operator = lexeme ((:) <$> opStart <*> many opLetter)

-- Identifiers
identStart :: Parser Char
identStart = validLetter <|> char '_'
  where
    validLetter = satisfy (\c -> isAlpha c && isAscii c)

identLetter :: Parser Char
identLetter = validAlphaNum <|> char '_'
  where
    validAlphaNum = satisfy (\c -> (isAlpha c || isDigit c) && isAscii c)

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> identStart <*> many identLetter
    check x =
      if x `elem` reservedWords
        then fail (x ++ " is reserved")
        else return x

lvalue :: Parser String
lvalue = try identifier <|> parens lvalue <?> "lvalue"
