module Compile.Dataflow.DFS () where

import Compile.IR (BBFunc, BasicBlock (lines, successors), IRBasicBlock, IStmt (GotoIfNot), Label)
import Control.Monad.State (State, evalState, gets, modify)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as Set

type Visited = Set.Set Label

data DfsState t s d = DfsState {visited :: Visited, blocks :: Map.Map Label (BasicBlock (IStmt t, s) d)}

type DFS t s d a = State (DfsState t s d) a

-- returns basic blocks in dfs order
-- orderGraph :: Map.Map Label (BasicBlock (IStmt t, s) d) -> Label -> [Label]
-- -- orderGraph = orderGrapgInternal Set.empty
-- orderGraph bs start = evalState (orderGraphInternal start) $ DfsState {visited = Set.empty, blocks = bs}

visit :: Label -> DFS t s d ()
visit label = modify (\s -> s {visited = Set.insert label . visited $ s})

orderGraphInternal :: Label -> DFS t s d [Label]
orderGraphInternal label = do
  is_visited <- gets (Set.member label . visited)
  if is_visited
    then pure []
    else do
      visit label
      block <- gets (fromJust . Map.lookup label . blocks)
      followers <- mapM orderGraphInternal (naturalSuccessorOrder block)
      pure $ label : concat followers

naturalSuccessorOrder :: BasicBlock (IStmt t, s) d -> [Label]
naturalSuccessorOrder block =
  assert "no basic block should have >2 successors" (length (successors block) <= 2) $
    assert "natural order should contain all successors" (length order == length (successors block)) $
      order
  where
    alt = case fmap fst . saveHead . reverse . Compile.IR.lines $ block of
      Just (GotoIfNot l _) -> Just l
      _ -> Nothing
    natural = saveHead . filter (\s -> Just s /= alt) $ successors block
    order = catMaybes [natural, alt]
    saveHead xs = if null xs then Nothing else Just $ head xs

assert :: String -> Bool -> a -> a
assert _ True = id
assert msg False = error msg
