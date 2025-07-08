{-# LANGUAGE TupleSections #-}

module Compile.Dataflow.RegAlloc (PhyRegister (..), allocateRegisters, usedRegs) where

import Compile.Dataflow.Coloring (Coloring, color, interferenceGraph)
import Compile.Dataflow.Liveness (addLiveness)
import Compile.IR
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (fromJust, mapMaybe)

data PhyRegister = PhyReg String | Stack Int | ArgStack Int
  deriving (Eq)

-- To make the code gen easer we leave some special registers unallocated:
notUse = map PhyReg ["rax", "rbx", "rcx", "rdx", "rbp", "rsp"]

-- This leaves
usedRegs = map PhyReg ["edi", "esi"] ++ map (PhyReg . (\n -> 'r' : n ++ "d") . show) [8 .. 15]

use = usedRegs ++ map Stack [1 ..]

-- which are ALL CALLEE SAVED.
-- We parse arguments via:
argumentRegs :: [PhyRegister]
argumentRegs = map PhyReg ["eax", "ebx", "ecx", "edx"] ++ map ArgStack [1 ..]

allocateRegisters :: (Ord t) => BBFunc t () -> (BBFunc PhyRegister (), Int)
allocateRegisters f = (,maxStack) $ mvArgs $ fmapSameSup (\x -> Map.findWithDefault (Stack 0) x regAssignment) f
  where
    c = color . interferenceGraph . addLiveness $ f
    regAssignment = assignRegisters c
    maxStack = maximum . (0 :) . mapMaybe stackNum $ Map.elems regAssignment :: Int
    stackNum r = case r of (Stack n) -> Just n; _ -> Nothing

assignRegisters :: (Ord t) => Coloring t -> Map.Map t PhyRegister
assignRegisters c = Map.mapMaybe (`Map.lookup` color2PhyRegister) c
  where
    color2PhyRegister = Map.fromList $ zip (sort $ Map.elems c) use

mvArgs :: BBFunc PhyRegister () -> BBFunc PhyRegister ()
mvArgs func = func {funcBlocks = Map.adjust prependMvs (funcName func) (funcBlocks func)}
  where
    prependMvs b = b {Compile.IR.lines = zipWith mv (funcArgs func) argumentRegs ++ Compile.IR.lines b}
    mv r a = (r :<- Reg a, ())

unsafeLookup :: (Ord k) => k -> Map.Map k a -> a
unsafeLookup k m = fromJust $ Map.lookup k m