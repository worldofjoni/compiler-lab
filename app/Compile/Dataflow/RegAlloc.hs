module Compile.Dataflow.RegAlloc (PhyReg, allocateRegisters) where

import Compile.Dataflow.Coloring (Color, Coloring, color, interferenceGraph)
import Compile.Dataflow.Liveness (LiveVars, addLiveness)
import Compile.IR
import qualified Data.Map as Map
import Data.Maybe (fromJust)

data PhyReg = PReg String | Stack Int | ArgStack Int

-- To make the code gen easer we leave some special registers unallocated:
notUse = map PReg ["rax", "rbx", "rcx", "rdx", "rbp", "rsp"]

-- This leaves
use = map PReg ["rdi", "rsi"] ++ map (PReg . ('r' :) . show) [8 .. 15] ++ map Stack [1 ..]

-- which are ALL CALLEE SAVED.
-- We parse arguments via:
argumentRegs :: [PhyReg]
argumentRegs = map PReg ["rax", "rbx", "rcx", "rdx"] ++ map ArgStack [1 ..]

allocateRegisters :: (Ord t) => BBFunc t () -> BBFunc PhyReg ()
allocateRegisters f = mvArgs $ fmapSameSup (`unsafeLookup` (assignRegisters c)) f
  where
    c = color . interferenceGraph . addLiveness $ f

assignRegisters :: (Ord t) => Coloring t -> Map.Map t PhyReg
assignRegisters = Map.mapMaybe (`Map.lookup` color2PhyReg)
  where
    color2PhyReg = Map.fromList $ zip [1 ..] use

mvArgs :: BBFunc PhyReg () -> BBFunc PhyReg ()
mvArgs func = func {funcBlocks = Map.adjust prependMvs (funcName func) (funcBlocks func)}
  where
    prependMvs b = b {Compile.IR.lines = zipWith mv (funcArgs func) argumentRegs ++ Compile.IR.lines b}
    mv r a = (r :<- Reg a, ())

unsafeLookup :: (Ord k) => k -> Map.Map k a -> a
unsafeLookup k m = fromJust $ Map.lookup k m