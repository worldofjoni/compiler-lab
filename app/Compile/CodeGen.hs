module Compile.CodeGen
  ( codeGen
  ) where

import           Compile.AST (AST(..), Expr(..), Stmt(..))

import           Control.Monad.State
import qualified Data.Map as Map
import Compile.IR


type VarName = String

type AAsmAlloc = Map.Map VarName VRegister

type CodeGen a = State CodeGenState a

data CodeGenState = CodeGenState
  { regMap :: AAsmAlloc
  , nextReg :: VRegister
  , code :: IR
  }

codeGen :: AST -> IR
codeGen (Block stmts _) = code $ execState (genBlock stmts) initialState
  where
    initialState = CodeGenState Map.empty 0 []

-- regName :: VRegister -> String
-- regName n = "%" ++ show n

freshReg :: CodeGen VRegister
freshReg = do
  curr <- get
  let r = nextReg curr
  put curr {nextReg = r + 1}
  return r

assignVar :: VarName -> VRegister -> CodeGen ()
assignVar name r = do
  modify $ \s -> s {regMap = Map.insert name r (regMap s)}

lookupVar :: VarName -> CodeGen VRegister
lookupVar name = do
  m <- gets regMap
  case Map.lookup name m of
    Just r -> return r
    Nothing -> error "unreachable, fix your semantic analysis I guess"

emit :: IStmt -> CodeGen ()
emit instr = modify $ \s -> s {code = code s ++ [instr]}

genBlock :: [Stmt] -> CodeGen ()
genBlock = mapM_ genStmt

genStmt :: Stmt -> CodeGen ()
genStmt (Decl name _) = do
  r <- freshReg
  assignVar name r
genStmt (Init name e _) = do
  rhs <- genExpr e
  r <- freshReg -- TODO: Maybe we don't need a fresh registers
  assignVar name r
  emit $ r :<- rhs
genStmt (Asgn name Nothing e _) = do -- normal assingment
  rhs <- genExpr e
  lhs <- lookupVar name
  emit $ lhs :<- rhs
genStmt (Asgn name (Just op) e _) = do
  rhs <- genExpr e
  lhs <- lookupVar name
  emit $ lhs :<-+ (Reg lhs, op, rhs)
genStmt (Ret e _) = do
  r <- genExpr e
  emit $ Return r

genExpr :: Expr -> CodeGen Operand
genExpr (IntExpr n _) = do
  -- r <- freshReg
  return (Imm n)
genExpr (Ident name _) = do
  r <- lookupVar name
  return (Reg r)
genExpr (UnExpr op e) = do
  r1 <- genExpr e
  r <- freshReg
  emit $ Unary r op r1
  return (Reg r)
genExpr (BinExpr op e1 e2) = do
  r1 <- genExpr e1
  r2 <- genExpr e2
  r <- freshReg
  emit $ r :<-+ (r1, op, r2)
  return (Reg r)
