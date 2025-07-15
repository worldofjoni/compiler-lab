{-# LANGUAGE InstanceSigs #-}

module Compile.AST where

import Data.List (intercalate)
import Text.Megaparsec

type AST = Program

type Program = [Definition]

type Ident = String

data Definition = Function Function | Struct StructDef

filterFunctions :: [Definition] -> [Function]
filterFunctions [] = []
filterFunctions (Function f : fs) = f : filterFunctions fs
filterFunctions (_ : fs) = filterFunctions fs

data StructDef = StructDef Ident [StructField]

type StructField = (Type, Ident)

data Function = Func Type Ident [(Type, Ident)] Block SourcePos

instance Show Function where
  show (Func ty name params code _) = show ty ++ " " ++ name ++ "(" ++ intercalate ", " (map show params) ++ ")\n" ++ show code

type Block = [Stmt]

data Type
  = IntType
  | BoolType
  | StructType Ident
  | PointerType Type
  | ArrayType Type
  | AnyPointer
  deriving (Ord)

isSmall :: Type -> Bool
isSmall IntType = True
isSmall BoolType = True
isSmall (PointerType _) = True
isSmall (ArrayType _) = True
isSmall (StructType _) = False
isSmall AnyPointer = True

instance Eq Type where
  (==) :: Type -> Type -> Bool
  AnyPointer == AnyPointer = True
  AnyPointer == (PointerType _) = True
  (PointerType _) == AnyPointer = True
  IntType == IntType = True
  BoolType == BoolType = True
  (PointerType t) == (PointerType u) = t == u
  (ArrayType t) == (ArrayType u) = t == u
  (StructType t) == (StructType u) = t == u
  _ == _ = False

isPointer :: Type -> Bool
isPointer (PointerType _) = True
isPointer AnyPointer = True
isPointer _ = False

isArray :: Type -> Bool
isArray (ArrayType _) = True
isArray _ = False

instance Show Type where
  show IntType = "int"
  show BoolType = "bool"
  show (StructType name) = "struct " ++ name
  show (PointerType t) = show t ++ "*"
  show (ArrayType t) = show t ++ "[]"
  show AnyPointer = "void*"

data Stmt
  = SimpStmt Simp
  | BlockStmt Block SourcePos
  | If Expr Stmt (Maybe Stmt) SourcePos
  | While Expr Stmt SourcePos
  | For (Maybe Simp) Expr (Maybe Simp) Stmt SourcePos
  | Break SourcePos
  | Continue SourcePos
  | Ret Expr SourcePos
  deriving (Show)

data LValue = Var Ident SourcePos | Field LValue Ident | Deref LValue | ArrayAccess LValue Expr
  deriving (Show)

lvalueToExpr :: LValue -> Expr
lvalueToExpr (Var n p) = VarExpr n p
lvalueToExpr (Field l i) = FieldE (lvalueToExpr l) i
lvalueToExpr (Deref l) = DerefE $ lvalueToExpr l
lvalueToExpr (ArrayAccess a e) = ArrayAccessE (lvalueToExpr a) e

data Simp
  = Decl Type String SourcePos
  | Init Type String Expr SourcePos
  | Asgn LValue AsgnOp Expr SourcePos
  | SimpCall String [Expr] SourcePos
  deriving (Show)

isDecl :: Simp -> Bool
isDecl (Decl {}) = True
isDecl (Init {}) = True
isDecl _ = False

data Expr
  = IntExpr String SourcePos
  | BoolExpr Bool SourcePos
  | Null SourcePos
  | VarExpr Ident SourcePos
  | FieldE Expr Ident
  | DerefE Expr
  | ArrayAccessE Expr Expr
  | BinExpr Expr Op Expr
  | UnExpr UnOp Expr
  | Ternary Expr Expr Expr
  | Call String [Expr] SourcePos
  | Alloc Type
  | AllocArray Type Expr
  deriving (Show)

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
  deriving (Eq)

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
  show Shl = "<<"
  show Shr = ">>"
  show BitOr = "|"
  show BitAnd = "&"
  show BitXor = "^"
  show Lt = "<="
  show Gt = ">="
  show Le = "<"
  show Ge = ">"
  show Eq = "=="
  show Neq = "!="
  show And = "&&"
  show Or = "||"

instance Show UnOp where
  show Neg = "-"
  show BitNot = "~"
  show Not = "!"

showAsgnOp :: AsgnOp -> String
showAsgnOp (Just op) = " " ++ show op ++ "= "
showAsgnOp _ = " = "
