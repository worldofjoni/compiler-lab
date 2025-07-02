module Compile.Dataflow.Liveness where

import Compile.IR
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

type LiveVars = Set.Set VRegister

type LivenessBlock = BasicBlock (IStmt, LiveVars)

type Liveness a = State LivenessState a

data LivenessState = LivenessState
  { blocks :: Map.Map Label LivenessBlock,
    liveBefore :: Map.Map Label LiveVars,
    liveAfter :: Map.Map Label LiveVars
  }

updateAllUntilConvergence :: [Label] -> Liveness ()
updateAllUntilConvergence order = do
  changed <- mapM update order
  Control.Monad.when (or changed) $ updateAllUntilConvergence order

update :: Label -> Liveness Bool
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

localLiveness :: LiveVars -> [IStmt] -> [LiveVars]
localLiveness inital = foldr f []
  where
    f stmt [] = [nowLive stmt inital]
    f stmt acc = (nowLive stmt . head $ acc) : acc

nowLive :: IStmt -> LiveVars -> LiveVars
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

changeIfReg :: (VRegister -> LiveVars -> LiveVars) -> Operand -> LiveVars -> LiveVars
changeIfReg f (Reg x) = f x
changeIfReg _ (Imm _) = id

deleteIfReg :: Operand -> LiveVars -> LiveVars
deleteIfReg = changeIfReg Set.delete

insertIfReg :: Operand -> LiveVars -> LiveVars
insertIfReg = changeIfReg Set.insert
