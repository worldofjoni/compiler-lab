module Compile.IR where

import Compile.AST (Op, UnOp)
import Data.List (intercalate)
import qualified Data.Map as Map

type VRegister = Int -- virtual register

type Label = String

type FrameSizes = Map.Map Label Int

type IR = [IRFunc]

type IRFunc = (String, Map.Map Label IRBasicBlock)
data BasicBlock line = BasicBlock {lines :: [line], successors :: [Label]}
type IRBasicBlock = BasicBlock IStmt

data Operand = Reg VRegister | Imm Integer

data IStmt
  = Return Operand
  | VRegister :<- Operand
  | VRegister :<-+ (Operand, Op, Operand)
  | Unary VRegister UnOp Operand
  | Goto Label
  | GotoIfNot Label Operand
  | CallIr (Maybe VRegister) Label [VRegister]
  | Nop

instance Show Operand where
  show (Reg r) = "vreg" ++ show r
  show (Imm n) = show n

instance Show IStmt where
  show (Return op) = "ret " ++ show op
  show (r :<- s) = show (Reg r) ++ " <- " ++ show s
  show (r :<-+ (s1, op, s2)) = show (Reg r) ++ " <- " ++ show s1 ++ " " ++ show op ++ " " ++ show s2
  show (Unary r op s) = show (Reg r) ++ " <- " ++ show op ++ show s
  show Nop = "nop"
  show (Goto l) = "goto " ++ show l
  show (GotoIfNot l op) = "goto " ++ show l ++ " if not " ++ show op
  show (CallIr tgt l regs) = maybe "" ((++ " <- ") . show . Reg) tgt ++ "call " ++ l ++ "(" ++ (intercalate ", " . map show) regs ++ ")"