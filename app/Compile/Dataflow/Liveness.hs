{-# LANGUAGE TupleSections #-}

module Compile.Dataflow.Liveness (LiveVars, LivenessBlock, LivenessFunc, addLiveness) where

import Compile.Dataflow.DFS (orderGraph)
import Compile.IR
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

type LiveVars t = Set.Set t

type LivenessBlock t = BasicBlock (IStmt t, LiveVars t) ()

type LivenessFunc t = BBFunc t (LiveVars t)

type Liveness t a = State (LivenessState t) a

data LivenessState t = LivenessState
  { blocks :: Map.Map Label (LivenessBlock t),
    cashedInputs :: Map.Map Label (LiveVars t)
  }
  deriving (Show)

addLiveness :: (Ord t) => BBFunc t () -> LivenessFunc t
addLiveness f =
  f
    { funcBlocks = blocks $ execState (updateAllUntilConvergence order) initialState
    }
  where
    order = orderGraph (funcBlocks f) (funcName f)
    addEmptyLiveVars b = b {Compile.IR.lines = map (\(x, ()) -> (x, Set.empty)) $ Compile.IR.lines b}
    initialState = LivenessState {blocks = fmap addEmptyLiveVars (funcBlocks f), cashedInputs = Map.empty}

updateAllUntilConvergence :: (Ord t) => [Label] -> Liveness t ()
updateAllUntilConvergence order = do
  changed <- mapM update order
  Control.Monad.when (or changed) $ updateAllUntilConvergence order

update :: (Ord t) => Label -> Liveness t Bool
update l = do
  bs <- gets blocks
  cis <- gets cashedInputs
  let b = unsafeLookup l bs
  let cashedInput = Map.lookup l cis
  input <- Set.unions <$> mapM outputOf (successors b)
  if cashedInput == Just input
    then return False
    else do
      let b' = b {Compile.IR.lines = addLocalLiveness input (map fst $ Compile.IR.lines b)}
      modify (\s -> s {blocks = Map.insert l b' (blocks s), cashedInputs = Map.insert l input (cashedInputs s)})
      return True

outputOf :: Label -> Liveness t (LiveVars t)
outputOf l = do
  ls <- gets (Compile.IR.lines . unsafeLookup l . blocks)
  return $ if null ls then Set.empty else snd . head $ ls

unsafeLookup :: (Ord k) => k -> Map.Map k a -> a
unsafeLookup k m = fromJust $ Map.lookup k m

addLocalLiveness :: (Ord t) => LiveVars t -> [IStmt t] -> [(IStmt t, LiveVars t)]
addLocalLiveness input stmts = zip stmts (localLiveness input stmts)

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
nowLive (Phi _ _) = error "todo: how does Phi impact liveness??"

changeIfReg :: (Ord t) => (t -> LiveVars t -> LiveVars t) -> Operand t -> LiveVars t -> LiveVars t
changeIfReg f (Reg x) = f x
changeIfReg _ (Imm _) = id

insertIfReg :: (Ord t) => Operand t -> LiveVars t -> LiveVars t
insertIfReg = changeIfReg Set.insert
