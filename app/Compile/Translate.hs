{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Compile.Translate
  ( translate,
  )
where

import Compile.AST
import Compile.IR
import Compile.Semantic.TypeCheck (StructDefs)
import Control.Monad.State
import Data.Foldable (traverse_)
import Data.List (sortOn)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Error (L1ExceptT)

type Translate = StateT TranslateState L1ExceptT

data TranslateState = TranslateState
  { nextReg :: VRegister,
    nextLabelNo :: Integer,
    loopEnds :: [Label],
    loopContinues :: [Label],
    labelOrder :: [Label],
    code :: Map.Map Label IRBasicBlock,
    currentLines :: [IStmt NameOrReg],
    currentLabel :: Label,
    currentFunc :: String,
    structDefs :: StructDefs
  }

translate :: StructDefs -> AST -> L1ExceptT IR
translate defs ast = catMaybes <$> mapM (genFunct defs) ast

freshReg :: Translate (Either a VRegister)
freshReg = do
  curr <- get
  let r = nextReg curr
  put curr {nextReg = r + 1}
  return (Right r)

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

genFunct :: StructDefs -> Definition -> L1ExceptT (Maybe (BBFunc NameOrReg ()))
genFunct defs (Function (Func _ name args block _)) = do
  endState <-
    execStateT
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
          labelOrder = [],
          structDefs = defs
        }
  pure . Just . reduceEmptyBlocks $
    BBFunc
      { funcName = name,
        funcArgs = map (Left . snd) args :: [NameOrReg],
        funcBlocks =
          fmap (fmapSameExtra (\s -> (s, ()))) . code $ endState,
        blockOrder = labelOrder endState
      }
genFunct _ (Struct _) = pure Nothing

reduceEmptyBlocks :: BBFunc a b -> BBFunc a b
reduceEmptyBlocks a@(BBFunc _ _ bs0 order) = a {funcBlocks = newBlocks, blockOrder = order'}
  where
    newBlocks = foldl reduce bs0 order
    order' = filter (`Map.member` newBlocks) order
    reduce :: Map.Map Label (BasicBlock (IStmt a, b) ()) -> Label -> Map.Map Label (BasicBlock (IStmt a, b) ())
    reduce bs label
      | null . Compile.IR.lines $ toDelete = Map.map (\v -> v {Compile.IR.lines = map (mapSnd updateJump) $ Compile.IR.lines v, successors = mapMaybe replace $ successors v}) . Map.delete label $ bs -- remove block
      | otherwise = bs
      where
        toDelete = bs Map.! label
        replace :: Label -> Maybe Label
        replace l
          | l == label = listToMaybe . successors $ toDelete
          | otherwise = Just l
        updateJump :: IStmt a -> IStmt a
        updateJump (Goto l) = maybe Nop Goto (replace l)
        updateJump (GotoIfNot l e) = maybe Nop (`GotoIfNot` e) (replace l)
        updateJump x = x
        mapSnd f (b, c) = (f b, c)

genBlock :: [Stmt] -> Translate ()
genBlock = mapM_ genStmt

genStmt :: Stmt -> Translate ()
genStmt (SimpStmt (Decl {})) = do
  pure ()
genStmt (SimpStmt (Init _ name e _)) = do
  assignTo (Left name) e
genStmt (SimpStmt (Asgn (Var name _) Nothing e _)) = do
  assignTo (Left name) e
genStmt (SimpStmt (Asgn (Var name _) (Just op) e _)) = do
  x <- toOperand e
  emit $ Left name :<-+ (Reg (Left name), op, x)
genStmt (SimpStmt (Asgn lv Nothing e _)) = do
  tmp <- freshReg
  assignTo tmp e
  addr <- getAddress (lvalueToExpr lv)
  emit $ addr :$<- Reg tmp
genStmt (SimpStmt (Asgn lv (Just op) e _)) = do
  ereg <- toOperand e
  addr <- getAddress $ lvalueToExpr lv
  tmp <- freshReg
  emit $ tmp :<-$ addr
  emit $ tmp :<-+ (Reg tmp, op, ereg)
  emit $ addr :$<- Reg tmp
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
  opsRegs <- mapM toOperand args
  emit $ CallIr Nothing name opsRegs

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
toOperand (VarExpr name _) = do
  return . Reg . Left $ name
toOperand e = do
  t <- freshReg
  assignTo t e
  return . Reg $ t

boolToInt :: Bool -> Integer
boolToInt True = 1
boolToInt False = 0

assignTo :: NameOrReg -> Expr -> Translate ()
assignTo d (IntExpr n _) = do
  emit $ d :<- Imm (read n)
assignTo d (BoolExpr b _) = do
  emit $ d :<- Imm (boolToInt b)
assignTo d (VarExpr name _) = do
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
  regs <- mapM toOperand args
  emit $ CallIr (Just d) name regs
assignTo d (Null _) = emit $ d :<- Imm 0
assignTo d (Alloc ty) = do
  tysize <- sizeof ty
  emit $ CallIr (Just d) "alloc" [Imm 1, Imm tysize]
assignTo d (AllocArray ty e) = do
  nElem <- toOperand e
  size <- freshReg
  tysize <- sizeof ty
  emit $ size :<-+ (nElem, Mul, Imm $ tysize `div` 8)
  emit $ size :<-+ (Reg size, Add, Imm 1) -- size accounts for 8 extra bytes for length of array
  emit $ CallIr (Just d) "alloc" [Reg size, Imm 8]
  emit $ (d, 0, Nothing, 0) :$<- nElem
assignTo d l = do
  -- DerefE, ArrayE, FieldE
  addr <- getAddress l
  emit $ d :<-$ addr

getAddress :: Expr -> Translate (Address NameOrReg)
getAddress (DerefE lv) = do
  r <- freshReg
  assignTo r lv
  pure (r, 0, Nothing, 0)
getAddress (ArrayAccessE lv idx) = do
  a <- freshReg
  assignTo a lv
  i <- freshReg
  assignTo i idx
  fill <- freshReg
  emit $ fill :<-$ (a, 0, Nothing, 0)
  emit $ AssertBounds i fill
  tysize <- sizeof undefined -- todo
  pure (a, tysize, Just i, 8)
getAddress (FieldE st fname) = do
  a <- freshReg
  assignTo a st
  offset <- structFieldOffset undefined fname -- todo
  pure (a, 0, Nothing, offset)
getAddress _ = error "var has no address, should never be called"

sizeof :: Type -> Translate Integer
sizeof IntType = pure 8 -- easier for now
sizeof BoolType = pure 8 -- easier for now
sizeof (PointerType _) = pure 8
sizeof AnyPointer = pure 8
sizeof (ArrayType _) = pure 8
sizeof (StructType name) = do
  struct <- gets ((Map.! name) . structDefs)
  let types = map snd . sortOn fst . Map.assocs $ struct
  sum . map (max 8) <$> mapM sizeof types -- allign all fields to 8 bytes

structFieldOffset :: Ident -> Ident -> Translate Integer
structFieldOffset sname fname = do
  struct <- gets ((Map.! sname) . structDefs)
  let types = map snd . takeWhile ((/= fname) . fst) . sortOn fst . Map.assocs $ struct
  sum . map (max 8) <$> mapM sizeof types
