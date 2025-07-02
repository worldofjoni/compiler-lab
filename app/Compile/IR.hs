module Compile.IR where

import Compile.AST (Op, UnOp)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Foldable (Foldable(toList))

type VRegister = Int -- virtual register

type Label = String

type FrameSizes = Map.Map Label Int

type BlockId = Int

type IR a = Map.Map BlockId (BasicBlock a)

data BasicBlock a = BasicBlock {blockId :: Int, stmts :: [(IStmt, a)], successors :: [BlockId]}

data Operand = Reg VRegister | Imm Integer


usedO :: Operand -> [VRegister]
usedO (Reg v) = [v]
usedO (Imm _) = []

data IStmt
  = Return Operand
  | VRegister :<- Operand
  | VRegister :<-+ (Operand, Op, Operand)
  | Unary VRegister UnOp Operand
  | Label Label
  | Goto Label
  | GotoIfNot Label Operand
  | CallIr (Maybe VRegister) Label [VRegister]
  | CallTail Label [VRegister]
  | FunctionLabel Label
  | Nop

used :: IStmt -> [VRegister]
used (Return o) = usedO o
used (_ :<- o) = usedO o
used (_ :<-+ (o, _, p)) = usedO o ++ usedO p
used (Unary _ _ o) = usedO o
used (Label _) = []
used (Goto _) = []
used (GotoIfNot _ o) = usedO o
used (CallIr _ _ rs) = rs
used (FunctionLabel _) = []
used Nop = []

defines :: IStmt -> [VRegister]
defines (r :<- _) = [r]
defines (r :<-+ _) = [r]
defines (Unary r _ _) = [r]
defines (CallIr r _ _) = toList r
defines _ = []

mapIR :: (a -> b) -> IR a -> IR b
mapIR f = Map.map (mapBlock f)

mapBlock :: (a -> b) -> BasicBlock a -> BasicBlock b
mapBlock f b = b {stmts = map (mapSnd f) (stmts b)}

mapSnd :: (t -> b) -> (a, t) -> (a, b)
mapSnd f (a, b) = (a, f b)

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
  show (Label l) = l ++ ":"
  show (CallIr tgt l regs) = maybe "" ((++ " <- ") . show . Reg) tgt ++ "call " ++ l ++ "(" ++ (intercalate ", " . map show) regs ++ ")"
  show (FunctionLabel label) = label ++ ":"
