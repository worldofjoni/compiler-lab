module Compile.CodeGen
  ( codeGen
  ) where

import           Compile.AST (AST(..), Expr(..), Stmt(..), showAsgnOp, Op)

import           Control.Monad.State
import qualified Data.Map as Map
import Compile.IR (VRegister)


type VarName = String

type AAsmAlloc = Map.Map VarName VRegister

type CodeGen a = State CodeGenState a

type IR = [IStmt]
data IStmt = Return VRegister | VRegister :<- VRegister | VRegister :<-+ (VRegister,  Op, VRegister)




data CodeGenState = CodeGenState
  { regMap :: AAsmAlloc
  , nextReg :: VRegister
  , code :: [String]
  }

codeGen :: AST -> [String]
codeGen (Block stmts _) = code $ execState (genBlock stmts) initialState
  where
    initialState = CodeGenState Map.empty 0 []

regName :: VRegister -> String
regName n = "%" ++ show n

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

emit :: String -> CodeGen ()
emit instr = modify $ \s -> s {code = code s ++ [instr]}

genBlock :: [Stmt] -> CodeGen ()
genBlock = mapM_ genStmt

genStmt :: Stmt -> CodeGen ()
genStmt (Decl name _) = do
  r <- freshReg
  assignVar name r
genStmt (Init name e _) = do
  r <- genExpr e
  assignVar name r
genStmt (Asgn name op e _) = do
  rhs <- genExpr e
  lhs <- lookupVar name
  emit $ regName lhs ++ showAsgnOp op ++ regName rhs
genStmt (Ret e _) = do
  r <- genExpr e
  emit $ "ret " ++ regName r

genExpr :: Expr -> CodeGen VRegister
genExpr (IntExpr n _) = do
  r <- freshReg
  emit $ regName r ++ " = " ++ show n
  return r
genExpr (Ident name _) = lookupVar name
genExpr (UnExpr op e) = do
  r1 <- genExpr e
  r <- freshReg
  emit $ regName r ++ " = " ++ show op ++ " " ++ regName r1
  return r
genExpr (BinExpr op e1 e2) = do
  r1 <- genExpr e1
  r2 <- genExpr e2
  r <- freshReg
  emit $ regName r ++ " = " ++ regName r1 ++ " " ++ show op ++ " " ++ regName r2
  return r
