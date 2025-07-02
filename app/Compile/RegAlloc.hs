module Compile.RegAlloc where

import Compile.IR (BasicBlock (..), IR, IStmt (Nop), VRegister, defines, mapIR, used)
import Control.Monad (foldM)
import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import qualified Data.IntMap as Map
import Data.List (sortOn)
import Data.Map (elems, toList, (!))
import qualified Data.Set as Set

allocateRegisters :: IR () -> IR ()
allocateRegisters = undefined

data LivenessState = LivenessState {blocks :: IR Live, hasChanged :: Bool}

type Lifeness = State LivenessState

type Live = Set.Set VRegister

livenessAnalysis :: IR () -> IR Live
livenessAnalysis ir = blocks $ execState analyze initial
  where
    initial = LivenessState (mapIR (const Set.empty) ir) False

analyze :: Lifeness ()
analyze = do
  bs <- gets (sortOn (negate . blockId) . elems . blocks)
  modify (\s -> s {blocks = blocks s})

blockLifeness :: IR Live -> BasicBlock Live -> BasicBlock Live
blockLifeness ir block = Map.update (\_ -> Just $ block {stmts = init . scanr update after . stmts $ block}) (blockId block) ir
  where
    update (stmt, prevLive) (_, oldLive) = (stmt, oldLive `Set.union` Set.fromList (used stmt) `Set.union` (prevLive `Set.difference` Set.fromList (defines stmt))) -- todo track updates
    after = (Nop, Set.unions . map (snd . head . stmts . (ir !)) . successors $ block)
