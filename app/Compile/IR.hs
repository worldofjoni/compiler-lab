module Compile.IR
  ( VRegister,
    IR,
    IStmt (..),
    Operand (..),
  )
where

import Compile.AST (Op, UnOp)
import GHC.Arr (Array)

type VRegister = Int -- virtual register

type LineNo = Int

type IR = Array LineNo IStmt

data Operand = Reg VRegister | Imm Integer

data IStmt
  = Return Operand
  | VRegister :<- Operand
  | VRegister :<-+ (Operand, Op, Operand)
  | Unary VRegister UnOp Operand
  | Goto LineNo
  | GotoIf LineNo Operand
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
  show (GotoIf l op) = "goto " ++ show l ++ " if " ++ show op
