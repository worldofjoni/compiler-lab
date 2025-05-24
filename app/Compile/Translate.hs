module Compile.Translate
  ( translate,
  )
where

import Compile.AST
import Compile.IR
import Control.Monad.State
import qualified Data.Map as Map
import GHC.Arr (array, numElements, (//))

type VarName = String

type AAsmAlloc = Map.Map VarName VRegister

type Translate a = State TranslateState a

data TranslateState = TranslateState
  { regMap :: AAsmAlloc,
    nextReg :: VRegister,
    code :: IR
  }

translate :: AST -> IR
translate stmts = code $ execState (genBlock stmts) initialState
  where
    initialState = TranslateState Map.empty 0 (array (0, 0) [])

freshReg :: Translate VRegister
freshReg = do
  curr <- get
  let r = nextReg curr
  put curr {nextReg = r + 1}
  return r

assignVar :: VarName -> VRegister -> Translate ()
assignVar name r = do
  modify $ \s -> s {regMap = Map.insert name r (regMap s)}

lookupVar :: VarName -> Translate VRegister
lookupVar name = do
  m <- gets regMap
  case Map.lookup name m of
    Just r -> return r
    Nothing -> error "unreachable, fix your semantic analysis I guess"

emit :: IStmt -> Translate ()
emit instr = modify $ \s -> s {code = code s // [(numElements $ code s, instr)]}

genBlock :: [Stmt] -> Translate ()
genBlock = mapM_ genStmt

genStmt :: Stmt -> Translate ()
genStmt (SimpStmt (Decl t name _)) = do
  r <- freshReg
  assignVar name r
genStmt (SimpStmt (Init t name e _)) = do
  r <- freshReg
  assignTo r e
  assignVar name r
genStmt (SimpStmt (Asgn name Nothing e _)) = do
  lhs <- lookupVar name
  assignTo lhs e
genStmt (SimpStmt (Asgn name (Just op) e _)) = do
  lhs <- lookupVar name
  x <- toOperand e
  emit $ lhs :<-+ (Reg lhs, op, x)
genStmt (Ret e _) = do
  x <- toOperand e
  emit $ Return x

toOperand :: Expr -> Translate Operand
toOperand (IntExpr n _) = pure . Imm . read $ n
toOperand (IdentExpr name _) = do
  r <- lookupVar name
  return $ Reg r
toOperand e = do
  t <- freshReg
  assignTo t e
  return $ Reg t

assignTo :: VRegister -> Expr -> Translate ()
assignTo d (IntExpr n _) = do
  emit $ d :<- Imm (read n)
assignTo d (IdentExpr name _) = do
  r <- lookupVar name
  emit $ d :<- Reg r
assignTo d (UnExpr op e) = do
  x <- toOperand e
  emit $ Unary d op x
assignTo d (BinExpr e1 op e2) = do
  x1 <- toOperand e1
  x2 <- toOperand e2
  emit $ d :<-+ (x1, op, x2)
