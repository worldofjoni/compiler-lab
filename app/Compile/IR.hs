module Compile.IR where

import Compile.AST (Op, UnOp)

newtype VRegister = VRegister Int deriving (Eq, Ord) -- virtual register

type Label = String

type IR = [IStmt]

data Operand = Reg VRegister | Imm Integer

data IStmt
  = Return Operand
  | VRegister :<- Operand
  | VRegister :<-+ (Operand, Op, Operand)
  | Unary VRegister UnOp Operand
  | Label Label
  | Goto Label
  | GotoIfNot Label Operand
  | Nop

instance Show Operand where
  show (Reg (VRegister r)) = "vreg" ++ show r
  show (Imm n) = show n

instance Show IStmt where
  show (Return op) = "ret " ++ show op
  show (r :<- s) = show (Reg r) ++ " <- " ++ show s
  show (r :<-+ (s1, op, s2)) = show (Reg r) ++ " <- " ++ show s1 ++ " " ++ show op ++ " " ++ show s2
  show (Unary r op s) = show (Reg r) ++ " <- " ++ show op ++ show s
  show Nop = "nop"
  show (Goto l) = "goto " ++ show l
  show (GotoIfNot l op) = "goto " ++ show l ++ " if not " ++ show op
  show (Label l) = l ++ ":"
