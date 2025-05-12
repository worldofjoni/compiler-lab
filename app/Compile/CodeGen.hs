module Compile.CodeGen
  ( codeGen,
  )
where

import Compile.AST (AST (..), Expr (..), Stmt (..))
import Compile.IR
import Control.Monad.State
import qualified Data.Map as Map

type VarName = String

type AAsmAlloc = Map.Map VarName VRegister

type CodeGen a = State CodeGenState a

data CodeGenState = CodeGenState
  { regMap :: AAsmAlloc,
    nextReg :: VRegister,
    code :: IR
  }

codeGen :: AST -> IR
codeGen (Block stmts _) = code $ execState (genBlock stmts) initialState
  where
    initialState = CodeGenState Map.empty 0 []

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
  r <- freshReg 
  assignTo r e
  assignVar name r
genStmt (Asgn name Nothing e _) = do
  lhs <- lookupVar name
  assignTo lhs e
genStmt (Asgn name (Just op) e _) = do
  lhs <- lookupVar name
  x <- toOperand e
  emit $ lhs :<-+ (Reg lhs, op, x)
genStmt (Ret e _) = do
  x <- toOperand e
  emit $ Return x

toOperand :: Expr -> CodeGen Operand
toOperand (IntExpr n _) = pure $ Imm n
toOperand (Ident name _) = do
  r <- lookupVar name
  return $ Reg r
toOperand e = do
  t <- freshReg
  assignTo t e
  return $ Reg t

assignTo :: VRegister -> Expr -> CodeGen ()
assignTo d (IntExpr n _) = do
  emit $ d :<- Imm n
assignTo d (Ident name _) = do
  r <- lookupVar name
  emit $ d :<- Reg r
assignTo d (UnExpr op e) = do
  x <- toOperand e
  emit $ Unary d op x
assignTo d (BinExpr op e1 e2) = do
  x1 <- toOperand e1
  x2 <- toOperand e2
  emit $ d :<-+ (x1, op, x2)
