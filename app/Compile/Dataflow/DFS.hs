module Compile.Dataflow.DFS (orderGraph) where

import Compile.IR (BasicBlock (successors), IRBasicBlock, Label)
import Control.Monad.State (State, evalState, gets, modify)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

type Visited = Set.Set Label

data DfsState = DfsState {visited :: Visited, blocks :: Map.Map Label IRBasicBlock}

type DFS a = State DfsState a

-- returns basic blocks in dfs order
orderGraph :: Map.Map Label IRBasicBlock -> Label -> [Label]
-- orderGraph = orderGrapgInternal Set.empty
orderGraph bs start = evalState (orderGrapgInternal start) $ DfsState {visited = Set.empty, blocks = bs}

visit :: Label -> DFS ()
visit label = modify (\s -> s {visited = Set.insert label . visited $ s})

orderGrapgInternal :: Label -> DFS [Label]
orderGrapgInternal label = do
  is_visited <- gets (Set.member label . visited)
  if is_visited
    then pure []
    else do
      visit label
      block <- gets (fromJust . Map.lookup label . blocks)
      followers <- traverse orderGrapgInternal (successors block)
      pure $ label : concat followers