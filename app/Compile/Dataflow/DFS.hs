module Compile.Dataflow.DFS (orderGraph) where

import Compile.IR (BasicBlock (successors), IRBasicBlock, Label, BBFunc)
import Control.Monad.State (State, evalState, gets, modify)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

type Visited = Set.Set Label

data DfsState t = DfsState {visited :: Visited, blocks :: Map.Map Label (BasicBlock t)}

type DFS t a = State (DfsState t) a

-- returns basic blocks in dfs order
orderGraph ::  Map.Map Label (BasicBlock t) -> Label -> [Label]
-- orderGraph = orderGrapgInternal Set.empty
orderGraph bs start = evalState (orderGraphInternal start) $ DfsState {visited = Set.empty, blocks = bs}

visit :: Label -> DFS t ()
visit label = modify (\s -> s {visited = Set.insert label . visited $ s})

orderGraphInternal :: Label -> DFS t [Label]
orderGraphInternal label = do
  is_visited <- gets (Set.member label . visited)
  if is_visited
    then pure []
    else do
      visit label
      block <- gets (fromJust . Map.lookup label . blocks)
      followers <- traverse orderGraphInternal (successors block)
      pure $ label : concat followers