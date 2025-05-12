module Compile.AsmGen (genAsm, genIStmt) where

import Compile.AST (Op (..), UnOp (..))
import Compile.IR (IR, IStmt (..), Operand (..), VRegister)

type Asm = String

genAsm :: Int -> IR -> Asm
genAsm numRegs = unlines . (preamble : {-. (initStack numRegs :)-}) . map genIStmt

-- initStack :: Int -> Asm
-- initStack numRegs = "sub" ++ decConst (numRegs * 4) ++ "%rsp" -- move stack pointer

genIStmt :: IStmt -> String
genIStmt (Return o) = unlines [mov (showOperand o) "%eax", "ret"]
genIStmt (x :<- (Imm i)) = mov (decConst i) (stackAddress x)
genIStmt (x :<- (Reg r)) = unlines [mov (stackAddress r) "%eax", mov "%eax" (stackAddress x)]
genIStmt (x :<-+ (a, Mul, b)) =
  unlines
    [ mov (showOperand a) "%eax",
      mov (showOperand b) "%ebx",
      "imul %eax, %ebx",
      mov "%ebx" (stackAddress x)
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
    [ mov (showOperand a) "%eax",
      "addl " ++ showOperand b ++ ", %eax",
      mov "%eax" (stackAddress x)
    ]
genIStmt (x :<-+ (a, Sub, b)) =
  unlines
    [ mov (showOperand a) "%eax",
      "subl " ++ showOperand b ++ ", %eax",
      mov "%eax" (stackAddress x)
    ]
genIStmt (Unary reg Neg a) =
  unlines
    [ mov (showOperand a) "%eax",
      "negl %eax",
      mov "%eax" (stackAddress reg)
    ]

stackAddress :: VRegister -> String
stackAddress reg = show (negate $ (reg + 1) * 4) ++ "(%rsp)"

showOperand :: Operand -> String
showOperand (Imm a) = decConst a
showOperand (Reg r) = stackAddress r

mov :: String -> String -> String
mov from to = "movl " ++ from ++ ", " ++ to

decConst :: Integer -> String
decConst i = '$' : show i

preamble :: String
preamble = ".global main\n.global _main\n.text\nmain:\ncall _main\n# move the return value into the first argument for the syscall\nmovq %rax, %rdi\n# move the exit syscall number into rax\nmovq $0x3C, %rax\nsyscall\n_main:"