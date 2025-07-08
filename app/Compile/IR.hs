{-# LANGUAGE InstanceSigs #-}

module Compile.IR where

import Compile.AST (Op, UnOp)
import Data.List (intercalate)
import qualified Data.Map as Map

type VRegister = Int -- virtual register

type Label = String

type VarName = String

type FrameSizes = Map.Map Label Int

type IR = [IRFunc]

showIRFunc :: (Show r, Show sup) => BBFunc r sup -> String
showIRFunc f =
  "function "
    ++ funcName f
    ++ "("
    ++ (join . map show . funcArgs) f
    ++ "):\n"
    ++ (unlines . map (\(n, l) -> "==> " ++ n ++ ":\n" ++ show l) . Map.toList) (funcBlocks f)
  where
    join [] = ""
    join [x] = x
    join (x : xs) = x ++ ", " ++ join xs

data BasicBlock line e = BasicBlock {lines :: [line], successors :: [Label], extra :: e}

instance (Show l, Show e) => Show (BasicBlock l e) where
  show :: (Show l, Show e) => BasicBlock l e -> String
  show (BasicBlock lns suc e) = (unlines . map show $ lns) ++ "\nSuccs:\n" ++ show suc ++ "\nextra:" ++ show e ++ "\n\n"

type NameOrReg = Either VarName VRegister

data BBFunc r sup = BBFunc {funcName :: String, funcArgs :: [r], funcBlocks :: Map.Map Label (BasicBlock (IStmt r, sup) ())}

type IRFunc = BBFunc NameOrReg ()

type IRBasicBlock = BasicBlock (IStmt NameOrReg) ()

data Operand a = Reg a | Imm Integer

data IStmt a
  = Return (Operand a)
  | a :<- (Operand a)
  | a :<-+ (Operand a, Op, Operand a)
  | Phi a [Operand a]
  | Unary a UnOp (Operand a)
  | Goto Label
  | GotoIfNot Label (Operand a)
  | CallIr (Maybe a) Label [a]
  | Nop

instance (Show a) => Show (Operand a) where
  show (Reg r) = "reg." ++ show r
  show (Imm n) = "imm." ++ show n

instance (Show a) => Show (IStmt a) where
  show (Return op) = "ret " ++ show op
  show (r :<- s) = show (Reg r) ++ " <- " ++ show s
  show (r :<-+ (s1, op, s2)) = show (Reg r) ++ " <- " ++ show s1 ++ " " ++ show op ++ " " ++ show s2
  show (Unary r op s) = show (Reg r) ++ " <- " ++ show op ++ show s
  show Nop = "nop"
  show (Goto l) = "goto " ++ show l
  show (GotoIfNot l op) = "goto " ++ show l ++ " if not " ++ show op
  show (CallIr tgt l regs) = maybe "" ((++ " <- ") . show . Reg) tgt ++ "call " ++ l ++ "(" ++ (intercalate ", " . map (show . Reg)) regs ++ ")"
  show (Phi tgt ls) = show tgt ++ " <- Φ(" ++ intercalate ", " (map show ls) ++ ")"

instance Functor Operand where
  fmap f (Reg x) = Reg (f x)
  fmap _ (Imm n) = Imm n

instance Functor IStmt where
  fmap f (Return op) = Return (fmap f op)
  fmap f (x :<- y) = f x :<- (fmap f y)
  fmap f (x :<-+ (a, op, b)) = f x :<-+ (fmap f a, op, fmap f b)
  fmap f (Phi a xs) = Phi (f a) (map (fmap f) xs)
  fmap f (Unary a op b) = Unary (f a) op (fmap f b)
  fmap _ (Goto l) = Goto l
  fmap f (GotoIfNot l x) = GotoIfNot l (fmap f x)
  fmap f (CallIr x l xs) = CallIr (fmap f x) l (fmap f xs)
  fmap _ Nop = Nop

fmapSameExtra :: (a -> b) -> BasicBlock a e -> BasicBlock b e
fmapSameExtra f b = b {Compile.IR.lines = map f (Compile.IR.lines b)}

fmapSameSup :: (a -> b) -> BBFunc a sup -> BBFunc b sup
fmapSameSup f func = func {funcArgs = map f $ funcArgs func, funcBlocks = fmapSameExtra (\(x, y) -> (f <$> x, y)) <$> funcBlocks func}