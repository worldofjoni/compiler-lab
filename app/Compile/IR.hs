module Compile.IR (
    VRegister, IR, IStmt(..), Operand(..)
    ) where
import Compile.AST (Op)

type VRegister = Int -- virtual register


type IR = [IStmt]
data Operand = Reg VRegister | Imm Integer 
data IStmt = Return VRegister | VRegister :<- Operand | VRegister :<-+ (Operand,  Op, Operand)


