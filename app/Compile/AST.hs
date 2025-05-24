module Compile.AST where

import Text.Megaparsec

type AST = Program

type Program = Block

type Block = [Stmt]

data Type = IntType | BoolType

data Stmt
  = SimpStmt Simp
  | BlockStmt Block SourcePos
  | If Expr Stmt (Maybe Stmt) SourcePos
  | While Expr Stmt SourcePos
  | For (Maybe Simp) Expr (Maybe Simp) Stmt SourcePos
  | Break SourcePos
  | Contiue SourcePos
  | Ret Expr SourcePos

data Simp
  = Decl Type String SourcePos
  | Init Type String Expr SourcePos
  | Asgn String AsgnOp Expr SourcePos

data Expr
  = IntExpr String SourcePos
  | BoolExpr Bool SourcePos
  | IdentExpr String SourcePos
  | BinExpr Expr Op Expr
  | UnExpr UnOp Expr
  | Ternary Expr Expr Expr

-- Nothing means =, Just is for +=, %=, ...
type AsgnOp = Maybe Op

data Op
  = Mul
  | Add
  | Sub
  | Div
  | Mod
  | Shl
  | Shr
  | BitOr
  | BitAnd
  | BitXor
  | Lt
  | Gt
  | Le
  | Ge
  | Eq
  | Neq
  | And
  | Or

data UnOp = Neg | Not | BitNot

-- re-exported for convenience
posPretty :: SourcePos -> String
posPretty = sourcePosPretty

-- Some very basic pretty printing
-- instance Show AST where
--   show (Block stmts _) =
--     "Block: {\n" ++ intercalate "\n" (map show stmts) ++ "\n}"

-- instance Show Stmt where
--   show (Decl name _) = "Decl: " ++ name
--   show (Init name e _) = "Init: " ++ name ++ " = " ++ show e
--   show (Asgn name op e _) =
--     "Assign: " ++ name ++ " " ++ show' op ++ " " ++ show e
--     where
--       show' (Just o) = show o ++ "="
--       show' Nothing = "="
--   show (Ret e _) = "Return: " ++ show e

-- instance Show Expr where
--   show (IntExpr i _) = i
--   show (Ident name _) = name
--   show (UnExpr op e) = "(" ++ show op ++ " " ++ show e ++ ")"
--   show (BinExpr op lhs rhs) =
--     "(" ++ show lhs ++ " " ++ show op ++ " " ++ show rhs ++ ")"

instance Show Op where
  show Mul = "*"
  show Add = "+"
  show Sub = "-"
  show Div = "/"
  show Mod = "%"

instance Show UnOp where
  show Neg = "-"

showAsgnOp :: AsgnOp -> String
showAsgnOp (Just op) = " " ++ show op ++ "= "
showAsgnOp _ = " = "
