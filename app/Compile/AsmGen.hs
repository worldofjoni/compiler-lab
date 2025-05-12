module Compile.AsmGen (genIR, genIStmt) where

import Compile.AST (Op (..))
import Compile.IR (IR, IStmt (..), Operand (..), VRegister)

type Asm = String

genIR :: Int -> IR -> Asm
genIR numRegs = unlines . (initStack numRegs :) . map genIStmt

initStack :: Int -> Asm
initStack numRegs = "sub" ++ decConst (numRegs * 4) ++ "%rsp" -- move stack pointer

genIStmt :: IStmt -> String
genIStmt (Return o) = unlines [mov (showOperand o) "%rax", "ret"]
genIStmt (x :<- o) = mov (showOperand o) (stackAddress x)
genIStmt (x :<-+ (a, Mul, b)) =
  unlines
    [ mov (showOperand a) "%eax",
      mov (showOperand b) "%ebx",
      "imul %eax, %ebx",
      mov "%eax" (stackAddress x)
    ]
genIStmt (x :<-+ (a, Div, b)) =
  unlines
    [ "xor %edx, %edx",
      mov (showOperand a) "%eax",
      mov (showOperand b) "%ecx",
      "idiv %ecx",
      mov "%eax" (stackAddress x)
    ]
genIStmt (x :<-+ (a, Mod, b)) =
  unlines
    [ "xor %edx, %edx",
      mov (showOperand a) "%eax",
      mov (showOperand b) "%ecx",
      "idiv %ecx",
      mov "%edx" (stackAddress x)
    ]
genIStmt (x :<-+ (a, Add, b)) =
  unlines
    [ mov (showOperand a) (stackAddress x),
      "add" ++ showOperand b ++ stackAddress x
    ]
genIStmt (x :<-+ (a, Sub, b)) =
  unlines
    [ mov (showOperand a) (stackAddress x),
      "sub" ++ showOperand b ++ stackAddress x
    ]
genIStmt (_ :<-+ (_, Neg, _)) = undefined

stackAddress :: VRegister -> String
stackAddress reg = show reg ++ "(%rsp)"

showOperand :: Operand -> String
showOperand (Imm a) = "$" ++ show a
showOperand (Reg r) = stackAddress r

mov :: String -> String -> String
mov from to = "mov " ++ from ++ ", " ++ to

decConst :: Int -> String
decConst i = '$' : show i
