module Compile.CodeGen (genAsm, genIStmt) where

import Compile.AST (Op (..), UnOp (..))
import Compile.IR (IR, IStmt (..), Label, Operand (..), VRegister)
import Data.Foldable (Foldable (toList))
import Data.List (unlines)

type Asm = String

genAsm :: Int -> IR -> Asm
genAsm numRegs = unlines . (preamble : {-. (initStack numRegs :)-}) . toList . fmap genIStmt

-- initStack :: Int -> Asm
-- initStack numRegs = "sub" ++ decConst (numRegs * 4) ++ "%rsp" -- move stack pointer

genIStmt :: IStmt -> String
genIStmt (Return o) = unlines [mov (showOperand o) "%eax", "ret"]
genIStmt (Label l) = unlines [l ++ ":"]
genIStmt (Goto l) = unlines ["jmp " ++ l]
genIStmt (GotoIfNot l b) = unlines [mov (showOperand b) "%ecx", "jnz " ++ l]
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
    [ mov (showOperand a) "%eax",
      "cdq",
      mov (showOperand b) "%ecx",
      "idiv %ecx",
      mov "%eax" (stackAddress x)
    ]
genIStmt (x :<-+ (a, Mod, b)) =
  unlines
    [ mov (showOperand a) "%eax",
      "cdq",
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
genIStmt (x :<-+ (a, Shl, b)) =
  unlines
    [ mov (showOperand a) "%eax",
      mov (showOperand b) "%ecx",
      "shl %eax, %ecx",
      mov "%eax" (stackAddress x)
    ]
genIStmt (x :<-+ (a, Shr, b)) =
  unlines
    [ mov (showOperand a) "%eax",
      mov (showOperand b) "%ecx",
      "sar %eax, %ecx",
      mov "%eax" (stackAddress x)
    ]
genIStmt (Unary reg Neg a) =
  unlines
    [ mov (showOperand a) "%eax",
      "negl %eax",
      mov "%eax" (stackAddress reg)
    ]

-- bitwise
genIStmt (x :<-+ (a, BitOr, b)) =
  unlines
    [ mov (showOperand a) "%eax",
      "or %eax, " ++ showOperand b,
      mov "%eax" (stackAddress x)
    ]
genIStmt (x :<-+ (a, BitAnd, b)) =
  unlines
    [ mov (showOperand a) "%eax",
      "and %eax, " ++ showOperand b,
      mov "%eax" (stackAddress x)
    ]
genIStmt (x :<-+ (a, BitXor, b)) =
  unlines
    [ mov (showOperand a) "%eax",
      "xor %eax, " ++ showOperand b,
      mov "%eax" (stackAddress x)
    ]
genIStmt (Unary x BitNot a) =
  unlines
    [ mov (showOperand a) (stackAddress x),
      "not " ++ stackAddress x
    ]

-- comparisons
genIStmt (x :<-+ (a, Lt, b)) = genCompare "setl" x a b
genIStmt (x :<-+ (a, Le, b)) = genCompare "setle" x a b
genIStmt (x :<-+ (a, Eq, b)) = genCompare "sete" x a b
genIStmt (x :<-+ (a, Neq, b)) = genCompare "setne" x a b
genIStmt (x :<-+ (a, Ge, b)) = genCompare "setqe" x a b
genIStmt (x :<-+ (a, Gt, b)) = genCompare "setg" x a b

-- logical
genIStmt (x :<-+ (a, And, b)) = genIStmt (x :<-+ (a, BitAnd, b))
genIStmt (x :<-+ (a, Or, b)) = genIStmt (x :<-+ (a, BitOr, b))
genIStmt (Unary x Not a) = genIStmt (Unary x BitNot a)

genIStmt Nop = ""

genCompare :: String -> VRegister -> Operand -> Operand -> String
genCompare setInst x a b =
  unlines
    [ mov (showOperand a) "%eax",
      "cmpl " ++ showOperand b ++ ", %eax",
      setInst ++ " %al",
      "movzbl %al, %eax",
      mov "%eax" (stackAddress x)
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