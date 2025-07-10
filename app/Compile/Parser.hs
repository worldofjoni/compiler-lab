module Compile.Parser
  ( parseAST,
    parseNumber,
  )
where

import Compile.AST
import Control.Applicative (asum)
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
  defs <- many parseDefinition
  eof
  return defs

parseDefinition :: Parser Definition
parseDefinition =
  (Struct <$> try parseStructDef)
    <|> (Function <$> parseFunction)

parseStructDef :: Parser StructDef
parseStructDef = do
  reserved "struct"
  name <- identifier
  members <- braces $ many $ (,) <$> parseType <*> identifier <* semi
  semi
  pure $ StructDef name members

parseFunction :: Parser Function
parseFunction = do
  pos <- getSourcePos
  retT <- parseType
  name <- identifier
  params <- nTuple $ (,) <$> parseType <*> identifier
  block <- parseBlock
  return $ Func retT name params block pos

parseCall :: Parser (String, [Expr], SourcePos)
parseCall = (,,) <$> funcIdentifier <*> nTuple expr <*> getSourcePos

nTuple :: Parser a -> Parser [a]
nTuple p = parens $ ((:) <$> p <*> many (symbol "," >> p)) <|> pure []

parseType' :: Parser Type
parseType' =
  (IntType <$ reserved "int")
    <|> (BoolType <$ reserved "bool")
    <|> (StructType <$ reserved "struct" <*> identifier)

parseType :: Parser Type
parseType =
  do
    t <- parseType'
    supps <- many (ArrayType <$ symbol "[]" <|> PointerType <$ symbol "*")
    pure $ foldl (flip ($)) t supps
    <?> "type"

stmt :: Parser Stmt
stmt =
  ( SimpStmt
      <$> simp
      <* semi
  )
    <|> parseIf
    <|> parseWhile
    <|> parseFor
    <|> parseContinue
    <|> parseBreak
    <|> ret
    <|> parseBlockStmt

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

parseBlock :: Parser Block
parseBlock = braces (many stmt)

parseBlockStmt :: Parser Stmt
parseBlockStmt = do
  pos <- getSourcePos
  block <- parseBlock
  return $ BlockStmt block pos

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
  Continue <$ reserved "continue" <* semi <*> return pos

parseBreak :: Parser Stmt
parseBreak = do
  pos <- getSourcePos
  Break <$ reserved "break" <* semi <*> return pos

simp :: Parser Simp
simp = try decl <|> try asign <|> (uncurry3 SimpCall <$> parseCall)

lvalue :: Parser LValue
lvalue =
  do
    lv <-
      try (flip Var <$> getSourcePos <*> identifier)
        <|> parens lvalue
        <|> (Deref <$ symbol "*" <*> lvalue)
    exts <-
      many $
        (flip Field <$ symbol "." <*> identifier)
          <|> ((\i l -> Field (Deref l) i) <$ symbol "->" <*> identifier)
          <|> (flip ArrayAccess <$> brackets expr)
    pure $ foldl (flip ($)) lv exts
    <?> "lvalue"

decl :: Parser Simp
decl = do
  pos <- getSourcePos
  type_ <- parseType
  name <- identifier
  maybeInit <- optional $ do
    void $ symbol "="
    expr
  return $ case maybeInit of
    Just e -> Init type_ name e pos
    Nothing -> Decl type_ name pos

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

tuple :: Parser a -> Parser b -> Parser (a, b)
tuple a b = parens ((,) <$> a <* symbol "," <*> b)

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

nullExpr :: Parser Expr
nullExpr = do
  pos <- getSourcePos
  reserved "NULL"
  return $ Null pos

opTable :: [[Operator Parser Expr]]
opTable =
  [ [Prefix (foldr1 (.) <$> some (DerefE <$ symbol "*"))],
    [manyUnaryOp [(Neg, "-"), (Not, "!"), (BitNot, "~")]],
    [infix_ Mul "*", infix_ Div "/", infix_ Mod "%"],
    [infix_ Add "+", infix_ Sub "-"],
    [infix_ Shl "<<", infix_ Shr ">>"],
    [infix_ Lt "<", infix_ Gt ">", infix_ Le "<=", infix_ Ge ">="],
    [infix_ Eq "==", infix_ Neq "!="],
    [infix_ BitAnd "&"],
    [infix_ BitXor "^"],
    [infix_ BitOr "|"],
    [infix_ And "&&"],
    [infix_ Or "||"],
    [TernR (Ternary <$ symbol ":" <$ symbol "?")] -- reversed order
  ]
  where
    -- this allows us to parse `---x` as `-(-(-x))`
    -- makeExprParser doesn't do this by default
    manyUnaryOp opsyms = Prefix $ foldr1 (.) <$> some (asum . map (\(op, sym) -> UnExpr op <$ symbol sym) $ opsyms)
    infix_ op sym = InfixL (flip BinExpr op <$ prefixSymbol sym)
    prefixSymbol n = (lexeme . try) (string n <* notFollowedBy opLetter)

expr :: Parser Expr
expr = makeExprParser expr' opTable <?> "expression"

expr' :: Parser Expr
expr' = do
  e <-
    intExpr
      <|> boolExpr
      <|> nullExpr
      <|> parens expr
      <|> try (uncurry AllocArray <$ reserved "alloc_array" <*> tuple parseType expr)
      <|> try (Alloc <$ reserved "alloc" <*> parens parseType)
      <|> try (uncurry3 Call <$> parseCall) -- need to come after special funcitons
      <|> try varExpr -- need to come after functions!
  exts <-
    many $
      (flip FieldE <$ symbol "." <*> identifier)
        <|> ((\i l -> FieldE (DerefE l) i) <$ symbol "->" <*> identifier)
        <|> (flip ArrayAccessE <$> brackets expr)
  pure $ foldl (flip ($)) e exts

varExpr :: Parser Expr
varExpr = flip VarExpr <$> getSourcePos <*> identifier

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

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

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

reservedString :: String -> Parser String
reservedString w = lexeme (string w <* notFollowedBy identLetter)

funcIdentifier :: Parser String
funcIdentifier = identifier <|> (choice . map reservedString $ reservedFunctions)

reservedFunctions :: [String]
reservedFunctions =
  [ "print",
    "read",
    "flush"
  ]

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
    "return",
    "string",
    "struct",
    "true",
    "void",
    "while"
  ]
    ++ reservedFunctions

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
