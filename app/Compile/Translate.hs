{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Compile.Translate
  ( translate,
  )
where

import Compile.AST
import Compile.IR
import Control.Monad.State
import Data.Foldable (traverse_)
import qualified Data.Map as Map

type Translate a = State TranslateState a

data TranslateState = TranslateState
  { nextReg :: VRegister,
    nextLabelNo :: Integer,
    loopEnds :: [Label],
    loopContinues :: [Label],
    labelOrder :: [Label],
    code :: Map.Map Label IRBasicBlock,
    currentLines :: [IStmt NameOrReg],
    currentLabel :: Label,
    currentFunc :: String
  }

translate :: AST -> IR
translate = map genFunct

freshReg :: Translate VRegister
freshReg = do
  curr <- get
  let r = nextReg curr
  put curr {nextReg = r + 1}
  return r

freshLabelWithPrefix :: String -> Translate Label
freshLabelWithPrefix prefix = do
  f <- gets currentFunc
  l <- freshLabel
  return $ f ++ "_" ++ prefix ++ l
  where
    freshLabel :: Translate Label
    freshLabel = do
      curr <- get
      let r = nextLabelNo curr
      put curr {nextLabelNo = r + 1}
      return $ show r

emit :: IStmt NameOrReg -> Translate ()
emit instr = modify $ \s -> s {currentLines = currentLines s ++ [instr]}

commitAndNew :: [Label] -> Label -> Translate ()
commitAndNew succs newLabel = modify $ \s -> s {labelOrder = labelOrder s ++ ifNotNull (currentLabel s), currentLines = [], currentLabel = newLabel, code = Map.insert (currentLabel s) (newBlock s) (code s)}
  where
    newBlock s = BasicBlock {Compile.IR.lines = currentLines s, successors = succs, extra = ()}
    ifNotNull "" = []
    ifNotNull l = [l]

pushLoopEnd :: Label -> Translate ()
pushLoopEnd l = modify $ \s -> s {loopEnds = l : loopEnds s}

popLoopEnd :: () -> Translate ()
popLoopEnd () = modify $ \s -> s {loopEnds = tail $ loopEnds s}

pushLoopContinue :: Label -> Translate ()
pushLoopContinue l = modify $ \s -> s {loopContinues = l : loopContinues s}

popLoopContinue :: () -> Translate ()
popLoopContinue () = modify $ \s -> s {loopContinues = tail $ loopContinues s}

-- -----------------------------------------------------------

genFunct :: Function -> BBFunc NameOrReg ()
genFunct (Func _ name args block _) =
  BBFunc
    { funcName = name,
      funcArgs = map (Left . snd) args :: [NameOrReg],
      funcBlocks =
        fmap (fmapSameExtra (\s -> (s, ()))) . code $ state,
      blockOrder = labelOrder state
    }
  where
    state =
      execState
        ( do
            genBlock block
            commitAndNew [] ""
        )
        TranslateState
          { nextReg = 0,
            nextLabelNo = 0,
            loopEnds = [],
            loopContinues = [],
            code = Map.empty,
            currentLines = [],
            currentLabel = name,
            currentFunc = name,
            labelOrder = []
          }

-- todo

genBlock :: [Stmt] -> Translate ()
genBlock = mapM_ genStmt

genStmt :: Stmt -> Translate ()
genStmt (SimpStmt (Decl {})) = do
  pure ()
genStmt (SimpStmt (Init _ name e _)) = do
  assignTo (Left name) e
genStmt (SimpStmt (Asgn name Nothing e _)) = do
  assignTo (Left name) e
genStmt (SimpStmt (Asgn name (Just op) e _)) = do
  x <- toOperand e
  emit $ Left name :<-+ (Reg (Left name), op, x)
genStmt (Ret e _) = do
  x <- toOperand e
  emit $ Return x
genStmt (If condition thenStmt (Just elseStmt) _) = do
  o <- toOperand condition
  elseLabel <- freshLabelWithPrefix "else"
  thenLabel <- freshLabelWithPrefix "then"
  endLabel <- freshLabelWithPrefix "endif"
  emit $ GotoIfNot elseLabel o
  commitAndNew [thenLabel, elseLabel] thenLabel
  genStmt thenStmt
  emit $ Goto endLabel
  commitAndNew [endLabel] elseLabel
  genStmt elseStmt
  emit $ Goto endLabel
  commitAndNew [endLabel] endLabel
genStmt (If condition thenStmt Nothing _) = do
  o <- toOperand condition
  endLabel <- freshLabelWithPrefix "endif"
  thenLabel <- freshLabelWithPrefix "then"
  emit $ GotoIfNot endLabel o
  commitAndNew [thenLabel, endLabel] thenLabel
  genStmt thenStmt
  commitAndNew [endLabel] endLabel
genStmt (While condition body _) = do
  loopLabel <- freshLabelWithPrefix "loop"
  loopCond <- freshLabelWithPrefix "loopCond"
  endLabel <- freshLabelWithPrefix "endloop"
  commitAndNew [loopCond] loopCond
  o <- toOperand condition
  emit $ GotoIfNot endLabel o
  commitAndNew [endLabel, loopLabel] loopLabel
  pushLoopEnd endLabel
  pushLoopContinue loopCond
  genStmt body
  popLoopEnd ()
  popLoopContinue ()
  emit $ Goto loopCond
  commitAndNew [loopCond] endLabel
genStmt (For initSimp condition after body _) = do
  loopLabel <- freshLabelWithPrefix "loop"
  loopCondLabel <- freshLabelWithPrefix "condloop"
  endLabel <- freshLabelWithPrefix "endloop"
  continueLabel <- freshLabelWithPrefix "continueloop"
  maybeGenSimp initSimp
  commitAndNew [loopCondLabel] loopCondLabel
  o <- toOperand condition
  emit $ GotoIfNot endLabel o
  commitAndNew [loopLabel, endLabel] loopLabel
  pushLoopContinue continueLabel
  pushLoopEnd endLabel
  genStmt body
  commitAndNew [continueLabel] continueLabel
  maybeGenSimp after
  popLoopContinue ()
  popLoopEnd ()
  emit $ Goto loopCondLabel
  commitAndNew [loopCondLabel] endLabel
genStmt (Break _) = do
  end <- gets (head . loopEnds)
  emit . Goto $ end
  commitAndNew [end] ""
genStmt (Continue _) = do
  cont <- gets (head . loopContinues)
  emit . Goto $ cont
  commitAndNew [cont] ""
genStmt (BlockStmt ss _) = traverse_ genStmt ss
genStmt (SimpStmt (SimpCall name args _)) = do
  opsRegs <-
    evalArgs args
  emit $ CallIr Nothing name opsRegs

evalArgs :: [Expr] -> Translate [NameOrReg]
evalArgs =
  mapM
    ( \e -> do
        reg <- freshReg
        assignTo (Right reg) e
        pure (Right reg)
    )

maybeGenStmt :: Maybe Stmt -> Translate ()
maybeGenStmt Nothing = pure ()
maybeGenStmt (Just s) = do
  _ <- genStmt s
  return ()

maybeGenSimp :: Maybe Simp -> Translate ()
maybeGenSimp = maybeGenStmt . fmap SimpStmt

toOperand :: Expr -> Translate (Operand NameOrReg)
toOperand (IntExpr n _) = pure . Imm . read $ n
toOperand (BoolExpr b _) = pure . Imm . boolToInt $ b
toOperand (IdentExpr name _) = do
  return . Reg . Left $ name
toOperand e = do
  t <- freshReg
  assignTo (Right t) e
  return . Reg . Right $ t

boolToInt :: Bool -> Integer
boolToInt True = 1
boolToInt False = 0

assignTo :: NameOrReg -> Expr -> Translate ()
assignTo d (IntExpr n _) = do
  emit $ d :<- Imm (read n)
assignTo d (BoolExpr b _) = do
  emit $ d :<- Imm (boolToInt b)
assignTo d (IdentExpr name _) = do
  emit $ d :<- Reg (Left name)
assignTo d (UnExpr op e) = do
  x <- toOperand e
  emit $ Unary d op x
assignTo d (BinExpr e1 And e2) = do
  shortLabel <- freshLabelWithPrefix "short"
  endLabel <- freshLabelWithPrefix "endshort"
  sndLabel <- freshLabelWithPrefix "snd"
  x1 <- toOperand e1
  emit $ GotoIfNot shortLabel x1
  commitAndNew [sndLabel, shortLabel] sndLabel
  x2 <- toOperand e2
  emit $ d :<- x2
  emit $ Goto endLabel
  commitAndNew [endLabel] shortLabel
  emit $ d :<- Imm 0
  commitAndNew [endLabel] endLabel
assignTo d (BinExpr e1 Or e2) = do
  longLabel <- freshLabelWithPrefix "long"
  endLabel <- freshLabelWithPrefix "endlong"
  sndLabel <- freshLabelWithPrefix "snd"
  x1 <- toOperand e1
  emit $ GotoIfNot longLabel x1
  commitAndNew [sndLabel, longLabel] sndLabel
  emit $ d :<- Imm 1
  emit $ Goto endLabel
  commitAndNew [endLabel] longLabel
  x2 <- toOperand e2
  emit $ d :<- x2
  commitAndNew [endLabel] endLabel
assignTo d (BinExpr e1 op e2) = do
  x1 <- toOperand e1
  x2 <- toOperand e2
  emit $ d :<-+ (x1, op, x2)
assignTo d (Ternary condition thenExpr elseExpr) = do
  elseLabel <- freshLabelWithPrefix "else"
  endLabel <- freshLabelWithPrefix "endif"
  thenLabel <- freshLabelWithPrefix "then"
  o <- toOperand condition
  emit $ GotoIfNot elseLabel o
  commitAndNew [thenLabel, elseLabel] thenLabel
  assignTo d thenExpr
  emit $ Goto endLabel
  commitAndNew [endLabel] elseLabel
  assignTo d elseExpr
  commitAndNew [endLabel] endLabel
assignTo d (Call name args _) = do
  regs <- evalArgs args
  emit $ CallIr (Just d) name regs