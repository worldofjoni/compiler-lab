{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Compile.IR where

import Compile.AST (Op, UnOp)
import Data.List (intercalate)
import qualified Data.Map as Map

type VRegister = Int -- virtual register

type Label = String

type VarName = String

type FrameSizes = Map.Map Label Int

type IR = [IRFunc]

type IRFunc = (String, Map.Map Label IRBasicBlock)

showIRFunc :: (Show l) => BBFunc l d -> [Char]
showIRFunc (name, blocks) = "function " ++ name ++ ":\n" ++ (unlines . map (\(n, l) -> "==> " ++ n ++ ":\n" ++ show l) . Map.toList) blocks

data BasicBlock line d = BasicBlock {lines :: [line], successors :: [Label], extra :: d}

instance (Show l) => Show (BasicBlock l d) where
  show :: (Show l) => BasicBlock l d -> String
  show (BasicBlock lns suc _) = (unlines . map show $ lns) ++ "\nSuccs:\n" ++ show suc ++ "\n\n"

type NameOrReg = Either VarName VRegister

type BBIR line d = [BBFunc line d]

type BBFunc line d = (String, Map.Map Label (BasicBlock line d))

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