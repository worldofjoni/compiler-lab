module Compile.Dataflow.DeadCode (eliminateDeadCode) where

import Compile.AST (Op (..))
import Compile.Dataflow.Liveness (LivenessFunc)
import Compile.IR
import qualified Data.Set as Set

eliminateDeadCode :: (Ord t) => LivenessFunc t -> LivenessFunc t
eliminateDeadCode = rmWriteToDead

rmWriteToDead :: (Ord t) => LivenessFunc t -> LivenessFunc t
rmWriteToDead = maybeMapStmts rmWrite . shiftLivenessUp
  where
    rmWrite (x :<- y, live) = if x `Set.member` live then Just (x :<- y, live) else Nothing
    rmWrite (s@(x :<-+ (a, op, b)), live)
      | x `Set.member` live = Just (s, live)
      | hasSideEffects op = Just (Operation (a, op, b), live)
      | otherwise = Nothing
    rmWrite s = Just s

shiftLivenessUp :: (Ord t) => LivenessFunc t -> LivenessFunc t
shiftLivenessUp f = f {funcBlocks = shiftBlock <$> funcBlocks f}
  where
    shiftBlock b = b {Compile.IR.lines = shift $ Compile.IR.lines b}
    shift ((s1, _) : ss@((_, l2) : _)) = (s1, l2) : shift ss
    shift [(s, _)] = [(s, Set.empty)]
    shift [] = []

hasSideEffects :: Op -> Bool
hasSideEffects = (`elem` [Mul, Div, Mod])
