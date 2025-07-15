{-# LANGUAGE TupleSections #-}

module Compile.Dataflow.SSA where

import Compile.Dataflow.Liveness (LiveVars)
import Compile.IR (BBFunc, BasicBlock (BasicBlock, extra, lines, successors), IStmt (..), Label, NameOrReg, Operand (Imm, Reg), VRegister, VarName)
import Control.Monad.State (State, evalState, execState, gets, modify, runState)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as Set

type VNameOrReg = Either (VarName, Int) VRegister -- versioned names

type VariableVersions = Map.Map VarName Int

data SSAState = SSAState {nextId :: VariableVersions}

getId :: VarName -> SSA (VarName, Int)
getId name = do
  gets ((name,) . fromJust . Map.lookup name . nextId)

getReg :: NameOrReg -> SSA VNameOrReg
getReg (Left name) = Left <$> getId name
getReg (Right i) = pure $ Right i

stepId :: NameOrReg -> SSA VNameOrReg
stepId (Left name) = do
  (_, iD) <- getId name
  modify (\s -> s {nextId = Map.insert name (iD + 1) $ nextId s})
  pure $ Left (name, iD + 1)
stepId (Right i) = pure $ Right i

type SSA a = State SSAState a

type LifeLine = (IStmt NameOrReg, LiveVars NameOrReg)

-- with lifeness
intoSSA :: BBFunc LifeLine () -> BBFunc (IStmt VNameOrReg) VariableVersions
intoSSA (name, bmap) = (name, populatePhi $ Map.fromList newMap)
  where
    bs = Map.toList bmap
    newMap = evalState (mapM action bs) initial
    initial = SSAState Map.empty
    action (nm, block) = (nm,) <$> ssaBlock block

type ID t = t -> t

type Phi = State (Map.Map Label (BasicBlock (IStmt VNameOrReg) VariableVersions))

populatePhi :: ID (Map.Map Label (BasicBlock (IStmt VNameOrReg) VariableVersions))
populatePhi mm = execState (mapM handleBlockPhi (Map.keys mm)) mm

handleBlockPhi :: Label -> Phi ()
handleBlockPhi label = do
  block <- gets (fromJust . Map.lookup label)
  let finalVersions = extra block
  let succs = successors block
  mapM_ (insertPhiBlock finalVersions) succs

insertPhiBlock :: VariableVersions -> Label -> Phi ()
insertPhiBlock versions label = do
  modify (Map.update (\b -> Just (b {Compile.IR.lines = map update $ Compile.IR.lines b})) label)
  where
    update (Phi (Left x) ls) = Phi (Left x) (ls ++ [Reg $ Left (fst x, fromJust $ Map.lookup (fst x) versions)])
    update x = x

ssaBlock :: BasicBlock LifeLine () -> SSA (BasicBlock (IStmt VNameOrReg) VariableVersions)
ssaBlock (BasicBlock ls scc _) = do
  let life = snd . head $ ls
  life' <- mapM stepId . Set.toList $ life
  let phis = map (`Phi` []) life'
  ll <- mapM (uniqueLine . fst) ls
  state <- gets nextId
  pure $ BasicBlock (phis ++ ll) scc state

uniqueLine :: IStmt NameOrReg -> SSA (IStmt VNameOrReg)
uniqueLine (Return o) = Return <$> getOp o
uniqueLine (x :<- o) = do
  op <- getOp o
  xx <- stepId x
  pure (xx :<- op)
uniqueLine (x :<-+ (o1, op, o2)) = do
  oo1 <- getOp o1
  oo2 <- getOp o2
  xx <- stepId x
  pure (xx :<-+ (oo1, op, oo2))
uniqueLine (Unary x op o) = do
  oo <- getOp o
  xx <- stepId x
  pure (Unary xx op oo)
uniqueLine (Goto l) = pure $ Goto l
uniqueLine (GotoIfNot l o) = GotoIfNot l <$> getOp o
uniqueLine (CallIr x l os) = do
  oos <- mapM getReg os
  xx <- mapM stepId x
  pure $ CallIr xx l oos
uniqueLine Nop = pure Nop
uniqueLine (Phi x os) = undefined

-- uniqueLine (a :<- o) = Return <$> getOp o

getOp :: Operand NameOrReg -> SSA (Operand VNameOrReg)
getOp (Imm i) = pure (Imm i)
getOp (Reg (Right t)) = pure $ Reg (Right t)
getOp (Reg (Left t)) = do
  iD <- getId t
  pure $ Reg (Left iD)

deSSA :: BBFunc (IStmt VNameOrReg) d -> BBFunc (IStmt Int) d
deSSA = undefined