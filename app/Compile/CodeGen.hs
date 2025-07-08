{-# LANGUAGE InstanceSigs #-}

module Compile.CodeGen (genAsm) where

import Compile.AST (Op (..), UnOp (..))
import Compile.Dataflow.RegAlloc (PhyRegister (..), usedRegs)
import Compile.IR (BBFunc (BBFunc), BasicBlock (lines), IStmt (..), Operand (..), fmapSameExtra)
import Data.Char (isDigit)
import Data.Foldable (Foldable (toList))
import qualified Data.Map as Map

type Asm = String

genAsm :: [(BBFunc PhyRegister a, Int)] -> Asm
genAsm = unlines . (preamble :) . toList . map (uncurry genFunc)
  where
    genFunc :: BBFunc PhyRegister a -> Int -> String
    genFunc (BBFunc name _ blocks) maxStack = unlines . (funcPreamble ++) . map (uncurry genBasicBlock) . Map.toList . Map.map (fmapSameExtra fst) $ blocks
      where
        funcPreamble =
          ["func_" ++ name ++ ":", "push %rbp", "mov %rsp, %rbp", "sub $" ++ show (maxStack * 4) ++ ", %rsp"]
            ++ ["push " ++ (show . var64) r | r <- usedRegs]
    genBasicBlock :: String -> BasicBlock (IStmt PhyRegister) a -> String
    genBasicBlock name b = "bb_" ++ name ++ ":\n" ++ (unlines . map genIStmt) (Compile.IR.lines b)
    genIStmt :: IStmt PhyRegister -> String
    -- genIStmt (Label l) = unlines [l ++ ":"]
    genIStmt (Goto l) = unlines ["jmp bb_" ++ l]
    genIStmt (GotoIfNot l b) = unlines [mov (showOperand b) "%ecx", "cmpl $0, %ecx", "je bb_" ++ l]
    genIStmt (x :<- (Imm i)) = mov (decConst i) (show x)
    genIStmt (x :<- (Reg r)) = unlines [mov (show r) "%eax", mov "%eax" (show x)]
    genIStmt (x :<-+ (a, Mul, b)) =
      unlines
        [ mov (showOperand a) "%eax",
          mov (showOperand b) "%ebx",
          "imul %eax, %ebx",
          mov "%ebx" (show x)
        ]
    genIStmt (x :<-+ (a, Div, b)) =
      unlines
        [ mov (showOperand a) "%eax",
          "cdq",
          mov (showOperand b) "%ecx",
          "idiv %ecx",
          mov "%eax" (show x)
        ]
    genIStmt (x :<-+ (a, Mod, b)) =
      unlines
        [ mov (showOperand a) "%eax",
          "cdq",
          mov (showOperand b) "%ecx",
          "idiv %ecx",
          mov "%edx" (show x)
        ]
    genIStmt (x :<-+ (a, Add, b)) =
      unlines
        [ mov (showOperand a) "%eax",
          "addl " ++ showOperand b ++ ", %eax",
          mov "%eax" (show x)
        ]
    genIStmt (x :<-+ (a, Sub, b)) =
      unlines
        [ mov (showOperand a) "%eax",
          "subl " ++ showOperand b ++ ", %eax",
          mov "%eax" (show x)
        ]
    genIStmt (x :<-+ (a, Shl, b)) =
      unlines
        [ mov (showOperand a) "%eax",
          mov (showOperand b) "%ecx",
          "shl %cl, %eax",
          mov "%eax" (show x)
        ]
    genIStmt (x :<-+ (a, Shr, b)) =
      unlines
        [ mov (showOperand a) "%eax",
          mov (showOperand b) "%ecx",
          "sar %cl, %eax",
          mov "%eax" (show x)
        ]
    genIStmt (Unary reg Neg a) =
      unlines
        [ mov (showOperand a) "%eax",
          "negl %eax",
          mov "%eax" (show reg)
        ]
    -- bitwise
    genIStmt (x :<-+ (a, BitOr, b)) =
      unlines
        [ mov (showOperand a) "%eax",
          "or " ++ showOperand b ++ ", %eax",
          mov "%eax" (show x)
        ]
    genIStmt (x :<-+ (a, BitAnd, b)) =
      unlines
        [ mov (showOperand a) "%eax",
          "and " ++ showOperand b ++ ", %eax",
          mov "%eax" (show x)
        ]
    genIStmt (x :<-+ (a, BitXor, b)) =
      unlines
        [ mov (showOperand a) "%eax",
          "xor " ++ showOperand b ++ ", %eax",
          mov "%eax" (show x)
        ]
    genIStmt (Unary x BitNot a) =
      unlines
        [ mov (showOperand a) "%eax",
          "not %eax",
          mov "%eax" (show x)
        ]
    -- comparisons
    genIStmt (x :<-+ (a, Lt, b)) = genCompare "setl" x a b
    genIStmt (x :<-+ (a, Le, b)) = genCompare "setle" x a b
    genIStmt (x :<-+ (a, Eq, b)) = genCompare "sete" x a b
    genIStmt (x :<-+ (a, Neq, b)) = genCompare "setne" x a b
    genIStmt (x :<-+ (a, Ge, b)) = genCompare "setge" x a b
    genIStmt (x :<-+ (a, Gt, b)) = genCompare "setg" x a b
    -- logical
    genIStmt (_ :<-+ (_, And, _)) = error "logical and should have been eliminated by translate"
    genIStmt (_ :<-+ (_, Or, _)) = error "logical or should have been eliminated by translate"
    genIStmt (Unary x Not a) =
      unlines
        [ mov (showOperand a) "%eax",
          "xor $1, %eax",
          mov "%eax" (show x)
        ]
    genIStmt Nop = ""
    genIStmt (Return o) = unlines $ [mov (showOperand o) "%eax"] ++ ["pop " ++ (show . var64) r | r <- usedRegs] ++ ["leave", "ret"]
    genIStmt (CallIr mret name regs) =
      unlines $
        ["push " ++ show r | r <- reverse regs]
          ++ ["call func_" ++ name]
          ++ maybe [] (pure . mov "%eax" . show) mret
          ++ ["pop %rax" | _ <- regs]
    genIStmt (Phi {}) = error "Phi nodes should be eliminated before code gen"

-- genIStmt (FunctionLabel name) =
-- unlines
-- ["func_" ++ name ++ ":", "push %rbp", "mov %rsp, %rbp", "sub $" ++ (show . (* 4) . fromJust . Map.lookup name $ sizes) ++ ", %rsp"]

genCompare :: String -> PhyRegister -> Operand PhyRegister -> Operand PhyRegister -> String
genCompare setInst x a b =
  unlines
    [ mov (showOperand a) "%eax",
      "cmpl " ++ showOperand b ++ ", %eax",
      setInst ++ " %al",
      "movzbl %al, %eax",
      mov "%eax" (show x)
    ]

instance Show PhyRegister where
  show :: PhyRegister -> String
  show (PhyReg r) = '%' : r
  show (Stack n) = show (-((length usedRegs + n) * 8)) ++ "(%rbp)"
  show (ArgStack n) = show ((n + 1) * 8 + 16) ++ "(%rbp)"

var64 :: PhyRegister -> PhyRegister
var64 (PhyReg x) = if isDigit $ x !! 1 then PhyReg (init $ x) else PhyReg ('r' : tail x)
var64 x = x

-- stackAddress :: Int -> String
-- stackAddress reg
--   | reg >= 0 = show (-(reg * 4)) ++ "(%rbp)" -- current stack frame: skip previous base pointer
--   | otherwise = show (-((reg + 1) * 8) + 16) ++ "(%rbp)" -- function parameters: previous frame: skip return address; parameters are pushed as 8 byte..

showOperand :: Operand PhyRegister -> String
showOperand (Imm a) = decConst a
showOperand (Reg r) = show r

mov :: String -> String -> String
mov from to = "movl " ++ from ++ ", " ++ to

decConst :: Integer -> String
decConst i = '$' : show i

preamble :: String
preamble =
  ".global main\n.global _main\n.text\nmain:\ncall func_main\n# move the return value into the first argument for the syscall\nmovq %rax, %rdi\n# move the exit syscall number into rax\nmovq $0x3C, %rax\nsyscall\n"
    ++ functions

functions :: String
functions =
  unlines
    [ "func_print:\nmov $1, %rax\n  mov $1, %rdi\n  lea 8(%rsp), %rsi\n  mov $1, %rdx\n  syscall\n  mov $0, %eax\n  ret\n",
      "func_read:\n  push %rax\n  mov $0, %rax\n  mov $0, %rdi\n  mov %rsp, %rsi\n  mov $1, %rdx\n  syscall\n  mov %rax, %rbx\n  pop %rax\n  and $0xFF, %rax\n  mov $-1, %edx\n  cmp $1, %rbx\n  cmovnz %edx, %eax\n  ret\n  ",
      "func_flush: \n   mov $0, %eax\n ret\n"
    ]
