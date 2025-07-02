{-# LANGUAGE TupleSections #-}

module Compile.Dataflow.Liveness where

import Compile.IR
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

type LiveVars t = Set.Set t

type LivenessBlock t = BasicBlock (IStmt t, LiveVars t)
type LivenessFunc t = BBFunc (IStmt t, LiveVars t)

type Liveness t a = State (LivenessState t) a

data LivenessState t = LivenessState
  { blocks :: Map.Map Label (LivenessBlock t),
    liveBefore :: Map.Map Label (LiveVars t),
    liveAfter :: Map.Map Label (LiveVars t)
  }

addLiveness :: (Ord t) => BBFunc (IStmt t) -> LivenessFunc t
addLiveness (name, bs) =
  ( name,
    blocks $ execState (updateAllUntilConvergence order) initialState
  )
  where
    order = Map.keys  bs
    emptyLiveVars = Map.fromList . map (,Set.empty) . Map.keys $ bs
    addEmptyLiveVars b = BasicBlock {Compile.IR.lines = map (,Set.empty) (Compile.IR.lines b), successors = successors b}
    initialState = LivenessState {blocks = fmap addEmptyLiveVars bs, liveAfter = emptyLiveVars, liveBefore = emptyLiveVars}

updateAllUntilConvergence :: (Ord t) => [Label] -> Liveness t ()
updateAllUntilConvergence order = do
  changed <- mapM update order
  Control.Monad.when (or changed) $ updateAllUntilConvergence order

update :: (Ord t) => Label -> Liveness t Bool
update l = do
  curr <- get
  let b = fromJust . Map.lookup l . blocks $ curr
  let ls = Compile.IR.lines b
  let succs = successors b
  let oldLiveAfter = fromJust . Map.lookup l . liveAfter $ curr
  let myLiveAfter = Set.unions . map (\s -> fromJust . Map.lookup s $ liveBefore curr) $ succs
  if myLiveAfter == oldLiveAfter
    then return False
    else do
      let stmts = map fst ls
      let lineLivenesses = localLiveness myLiveAfter stmts
      let updatedLines = zip stmts lineLivenesses
      let b' = b {Compile.IR.lines = updatedLines}
      put curr {blocks = Map.insert l b' (blocks curr)}
      put curr {liveAfter = Map.insert l (head lineLivenesses) (liveAfter curr)}
      return True

localLiveness :: (Ord t) => LiveVars t -> [IStmt t] -> [LiveVars t]
localLiveness inital = foldr f []
  where
    f stmt [] = [nowLive stmt inital]
    f stmt acc = (nowLive stmt . head $ acc) : acc

nowLive :: (Ord t) => IStmt t -> LiveVars t -> LiveVars t
nowLive (x :<- Imm _) = Set.delete x
nowLive (x :<- Reg y) = Set.insert y . Set.delete x
nowLive (x :<-+ (a, _, b)) = insertIfReg a . insertIfReg b . Set.delete x
nowLive (Unary x _ a) = insertIfReg a . Set.delete x
nowLive Nop = id
nowLive (CallIr (Just x) _ params) = Set.union (Set.fromList params) . Set.delete x
nowLive (CallIr Nothing _ params) = Set.union (Set.fromList params)
nowLive (Goto _) = id
nowLive (GotoIfNot _ _) = id
nowLive (Return (Imm _)) = const Set.empty
nowLive (Return (Reg x)) = const $ Set.singleton x
nowLive (Phi _ _) = error "todo: how does Phi affect liveness"

changeIfReg :: (Ord t) => (t -> LiveVars t -> LiveVars t) -> Operand t -> LiveVars t -> LiveVars t
changeIfReg f (Reg x) = f x
changeIfReg _ (Imm _) = id

deleteIfReg :: (Ord t) => Operand t -> LiveVars t -> LiveVars t
deleteIfReg = changeIfReg Set.delete

insertIfReg :: (Ord t) => Operand t -> LiveVars t -> LiveVars t
insertIfReg = changeIfReg Set.insert
