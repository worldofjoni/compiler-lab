module Compile.Translate
  ( translate,
  )
where

import Compile.AST
import Compile.IR
import Control.Monad.State
import qualified Data.Map as Map

type VarName = String

type AAsmAlloc = Map.Map VarName VRegister

type Translate a = State TranslateState a

data TranslateState = TranslateState
  { regMap :: AAsmAlloc,
    nextReg :: VRegister,
    nextLabelNo :: Integer,
    loopEnds :: [Label],
    loopContinues :: [Label],
    code :: IR
  }

translate :: AST -> IR
translate stmts = code $ execState (genBlock stmts) initialState
  where
    initialState = TranslateState Map.empty 0 0 [] [] []

freshReg :: Translate VRegister
freshReg = do
  curr <- get
  let r = nextReg curr
  put curr {nextReg = r + 1}
  return r

freshLabel :: Translate Label
freshLabel = do
  curr <- get
  let r = nextLabelNo curr
  put curr {nextLabelNo = r + 1}
  return $ show r

freshLabelWithPrefix :: String -> Translate Label
freshLabelWithPrefix prefix = do
  l <- freshLabel
  return $ prefix ++ l

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
emit instr = modify $ \s -> s {code = code s ++ [instr]}

pushLoopEnd :: Label -> Translate ()
pushLoopEnd l = modify $ \s -> s {loopEnds = l : loopEnds s}
popLoopEnd :: () -> Translate ()
popLoopEnd () = modify $ \s -> s {loopEnds = tail $ loopEnds s}

pushLoopContinue :: Label -> Translate ()
pushLoopContinue l = modify $ \s -> s {loopContinues = l : loopContinues s}
popLoopContinue :: () -> Translate ()
popLoopContinue () = modify $ \s -> s {loopContinues = tail $ loopContinues s}

genBlock :: [Stmt] -> Translate ()
genBlock = mapM_ genStmt

genStmt :: Stmt -> Translate ()
genStmt (SimpStmt (Decl _ name _)) = do
  r <- freshReg
  assignVar name r
genStmt (SimpStmt (Init _ name e _)) = do
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
genStmt (If condition thenStmt (Just elseStmt) _) = do
  o <- toOperand condition
  elseLabel <- freshLabelWithPrefix "else"
  endLabel <- freshLabelWithPrefix "endif"
  emit $ GotoIfNot elseLabel o
  genStmt thenStmt
  emit $ Goto endLabel
  emit $ Label elseLabel
  genStmt elseStmt
  emit $ Label endLabel
genStmt (If condition thenStmt Nothing _) = do
  o <- toOperand condition
  endLabel <- freshLabelWithPrefix "endif"
  emit $ GotoIfNot endLabel o
  genStmt thenStmt
  emit $ Label endLabel
genStmt (While condition body _) = do
  loopLabel <- freshLabelWithPrefix "loop"
  endLabel <- freshLabelWithPrefix "endloop"
  emit $ Label loopLabel
  o <- toOperand condition
  emit $ GotoIfNot endLabel o
  pushLoopEnd endLabel
  pushLoopContinue loopLabel
  genStmt body
  popLoopEnd ()
  popLoopContinue ()
  emit $ Goto loopLabel
  emit $ Label endLabel
genStmt (For initSimp condition after body _) = do
  loopLabel <- freshLabelWithPrefix "loop"
  endLabel <- freshLabelWithPrefix "endloop"
  continueLabel <- freshLabelWithPrefix "continueloop"
  maybeGenSimp initSimp
  emit $ Label loopLabel
  pushLoopContinue continueLabel
  pushLoopEnd endLabel
  o <- toOperand condition
  emit $ GotoIfNot endLabel o
  genStmt body
  emit $ Label continueLabel
  maybeGenSimp after
  emit $ Goto loopLabel
  emit $ Label endLabel
  popLoopContinue ()
  popLoopEnd ()
genStmt (Break _) = do
  curr <- get
  emit . Goto . head . loopEnds $ curr
genStmt (Continue _) = do
  curr <- get
  emit . Goto . head . loopContinues $ curr
genStmt (BlockStmt [] _) = pure ()
genStmt (BlockStmt (x:xs) sourcePos) = do
  genStmt x
  genStmt (BlockStmt xs sourcePos)
  


maybeGenStmt :: Maybe Stmt -> Translate ()
maybeGenStmt Nothing = pure ()
maybeGenStmt (Just s) = do 
  _ <- genStmt s
  return ()

maybeGenSimp :: Maybe Simp -> Translate ()
maybeGenSimp = maybeGenStmt . fmap SimpStmt

toOperand :: Expr -> Translate Operand
toOperand (IntExpr n _) = pure . Imm . read $ n
toOperand (BoolExpr b _) = pure . Imm . boolToInt $ b
toOperand (IdentExpr name _) = do
  r <- lookupVar name
  return $ Reg r
toOperand e = do
  t <- freshReg
  assignTo t e
  return $ Reg t

boolToInt:: Bool -> Integer
boolToInt True = 1
boolToInt False = 0

assignTo :: VRegister -> Expr -> Translate ()
assignTo d (IntExpr n _) = do
  emit $ d :<- Imm (read n)
assignTo d (BoolExpr b _) = do
  emit $ d :<- Imm (boolToInt b)
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
assignTo d (Ternary condition thenExpr elseExpr) = do
  elseLabel <- freshLabelWithPrefix "else"
  endLabel <- freshLabelWithPrefix "endif"
  o <- toOperand condition
  emit $ GotoIfNot elseLabel o
  assignTo d thenExpr
  emit $ Goto endLabel
  emit $ Label elseLabel
  assignTo d elseExpr
  emit $ Label endLabel