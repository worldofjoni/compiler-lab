module Compile.AST
  ( AST (..),
    Stmt (..),
    Expr (..),
    Op (..),
    UnOp (..),
    showAsgnOp,
    posPretty,
  )
where

import Data.Bifunctor (Bifunctor)
import Data.Bits (Xor)
import Data.List (intercalate)
import GHC.IO.Handle (LockMode (SharedLock))
import Options.Applicative.Help (SimpleDoc)
import Text.Megaparsec

type AST = Program

type Program = Block

data Block = Block [Stmt] SourcePos

data Type = IntType | BoolType

data Stmt
  = SimpStmt Simp SourcePos
  | BlockStmt Block SourcePos
  | If Expr Stmt (Maybe Stmt) SourcePos
  | While Expr Stmt
  | For (Maybe Simp) Expr (Maybe Simp) Stmt SourcePos
  | Break SourcePos
  | Contiue SourcePos
  | Return SourcePos

data Simp
  = Decl Type SourcePos
  | Init Type Expr SourcePos
  | Asign LValue AsgnOp Expr

data LValue = Ident String SourcePos

data Expr
  = IntExpr String SourcePos
  | BoolExpr Bool SourcePos
  | IdentExpr String SourcePos
  | Binop Expr Op Expr SourcePos
  | Unop UnOp Expr SourcePos
  | Trinay Expr Expr Expr SourcePos

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
