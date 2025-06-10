{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Compile.CodeGen (genAsm, genIStmt) where

import Compile.AST (Op (..), UnOp (..))
import Compile.IR (IR, IStmt (..), Operand (..), VRegister (VRegister))
import Control.Monad.State (State, execState, gets, modify)
import Data.Cache.LRU (LRU, fromList, insert, pop)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Data.Maybe (fromJust)

type Asm = String

data PRegister = PRegister Int deriving (Eq, Ord)

numPhysRegs :: Int
numPhysRegs = 8

instance Show PRegister where
  show :: PRegister -> String
  show (PRegister i) = "%r" ++ show (i + 8) ++ "d"

data CodegGenState = CodegGenState
  { regCache :: LRU PRegister (Maybe VRegister),
    regMap :: Map.Map VRegister PRegister,
    output :: [String]
  }

initalState :: CodegGenState
initalState =
  CodegGenState
    { regCache = fromList (Just $ toInteger numPhysRegs) . map ((,Nothing) . PRegister) $ [0 .. numPhysRegs],
      regMap = Map.empty,
      output = [preamble]
    }

type CodeGen = State CodegGenState

genAsm :: Int -> IR -> Asm
genAsm _numRegs ir = unlines . output . execState (mapM_ genIStmt ir) $ initalState

emitAll :: [String] -> CodeGen ()
emitAll emit = modify (\s -> s {output = output s ++ emit})

getRegister :: VRegister -> CodeGen PRegister
getRegister vReg = do
  cached <- gets (Map.lookup vReg . regMap)
  case cached of
    Just pReg -> pure pReg
    Nothing -> loadToCache vReg

saveResult :: String -> VRegister -> CodeGen ()
saveResult source vReg = do
  cached <- gets (Map.lookup vReg . regMap)
  case cached of
    Just pReg -> emitAll [mov source (show pReg)]
    Nothing -> do
      pReg <- loadToCache vReg
      emitAll [mov source (show pReg)]

loadToCache :: VRegister -> CodeGen PRegister
loadToCache vReg = do
  cache <- gets regCache
  let (cache', maybeReg) = pop cache
  let (pReg, storeVReg) = fromJust maybeReg -- cache is never empty
  traverse_ (store pReg) storeVReg -- save old cache value
  load vReg pReg -- load new value
  let cache'' = insert pReg (Just vReg) cache'
  modify (\r -> r {regCache = cache'', regMap = Map.insert vReg pReg . Map.filterWithKey (\a _ -> Just a /= storeVReg) . regMap $ r})
  pure pReg

getOperandOrImm :: Operand -> CodeGen String
getOperandOrImm (Reg r) = getRegister r <&> show
getOperandOrImm (Imm i) = pure $ decConst i

-- ensures operand is in _some_ register, if immediate move to the provided one
getOperandReg :: Operand -> String -> CodeGen String
getOperandReg (Reg r) _ = getRegister r <&> show
getOperandReg (Imm i) immReg = do
  emitAll [mov (decConst i) immReg]
  pure immReg

-- afterwards operand is definately in specified register
getOperandEnsureReg :: Operand -> String -> CodeGen ()
getOperandEnsureReg (Reg r) r2 = do
  regA <- getRegister r
  emitAll [mov (show regA) r2]
getOperandEnsureReg (Imm i) r2 = emitAll [mov (decConst i) r2]

eax :: String
eax = "%eax"

ebx :: String
ebx = "%ebx"

ecx :: String
ecx = "%ecx"

edx :: String
edx = "%edx"

store :: PRegister -> VRegister -> CodeGen ()
store pReg vReg = emitAll [mov (show pReg) (stackAddress vReg)]

load :: VRegister -> PRegister -> CodeGen ()
load vReg pReg = emitAll [mov (stackAddress vReg) (show pReg)]

stackAddress :: VRegister -> String
stackAddress (VRegister reg) = show (negate $ (reg + 1) * 4) ++ "(%rsp)"

-- todo optomize noop moves (make state monad action)
mov :: String -> String -> String
mov from to = "movl " ++ from ++ ", " ++ to

decConst :: Integer -> String
decConst i = '$' : show i

-- initStack :: Int -> Asm
-- initStack numRegs = "sub" ++ decConst (numRegs * 4) ++ "%rsp" -- move stack pointer

genIStmt :: IStmt -> CodeGen ()
genIStmt (Return o) = do
  reg <- getOperandOrImm o
  emitAll [mov reg "%eax", "ret"]
genIStmt (Label l) = emitAll [l ++ ":"]
genIStmt (Goto l) = emitAll ["jmp " ++ l]
genIStmt (GotoIfNot l b) = do
  reg <- getOperandOrImm b
  emitAll [mov reg "%ecx", "cmpl $0, %ecx", "je " ++ l]
genIStmt (x :<- (Imm i)) = do
  reg <- getRegister x
  emitAll [mov (decConst i) (show reg)]
genIStmt (x :<- (Reg r)) = do
  regR <- getRegister r
  regX <- getRegister x
  emitAll [mov (show regR) (show regX)]
genIStmt (x :<-+ (a, Mul, b)) = do
  regA <- getOperandReg a eax
  regB <- getOperandReg b ebx
  emitAll
    ["imul " ++ regA ++ ", " ++ regB]
  saveResult regB x
genIStmt (x :<-+ (a, Div, b)) = do
  getOperandEnsureReg a eax
  getOperandEnsureReg b ecx
  emitAll
    [ "cdq",
      "idiv %ecx"
    ]
  saveResult eax x
genIStmt (x :<-+ (a, Mod, b)) = do
  getOperandEnsureReg a eax
  getOperandEnsureReg b ecx
  emitAll
    [ "cdq",
      "idiv %ecx"
    ]
  saveResult edx x
genIStmt (x :<-+ (a, Add, b)) = do
  regA <- getOperandReg a eax
  regB <- getOperandOrImm b
  emitAll ["addl " ++ regB ++ ", " ++ regA]
  saveResult regA x
genIStmt (x :<-+ (a, Sub, b)) = do
  regA <- getOperandReg a eax
  regB <- getOperandOrImm b
  emitAll ["subl " ++ regB ++ ", " ++ regA]
  saveResult regA x
genIStmt (x :<-+ (a, Shl, b)) = do
  regA <- getOperandReg a eax
  getOperandEnsureReg b ecx
  emitAll ["shl %cl, " ++ regA]
  saveResult regA x
genIStmt (x :<-+ (a, Shr, b)) = do
  regA <- getOperandReg a eax
  getOperandEnsureReg b ecx
  emitAll ["sar %cl, " ++ regA]
  saveResult regA x
genIStmt (Unary reg Neg a) = do
  regA <- getOperandReg a eax
  emitAll ["negl " ++ regA]
  saveResult regA reg
-- bitwise
genIStmt (x :<-+ (a, BitOr, b)) = do
  regA <- getOperandReg a eax
  regB <- getOperandOrImm b
  emitAll ["or " ++ regB ++ ", " ++ regA]
  saveResult regA x
genIStmt (x :<-+ (a, BitAnd, b)) = do
  regA <- getOperandReg a eax
  regB <- getOperandOrImm b
  emitAll ["and " ++ regB ++ ", " ++ regA]
  saveResult regA x
genIStmt (x :<-+ (a, BitXor, b)) = do
  regA <- getOperandReg a eax
  regB <- getOperandOrImm b
  emitAll ["xor " ++ regB ++ ", " ++ regA]
  saveResult regA x
genIStmt (Unary x BitNot a) = do
  regA <- getOperandReg a eax
  emitAll ["not " ++ regA]
  saveResult regA x
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
genIStmt (Unary x Not a) = do
  regA <- getOperandReg a eax
  emitAll ["xor $1, " ++ regA]
  saveResult regA x
genIStmt Nop = pure ()

genCompare :: String -> VRegister -> Operand -> Operand -> CodeGen ()
genCompare setInst x a b = do
  getOperandEnsureReg a eax
  regB <- getOperandOrImm b
  emitAll
    [ "cmpl " ++ regB ++ ", %eax",
      setInst ++ " %al",
      "movzbl %al, %eax"
    ]
  saveResult eax x

preamble :: String
preamble = ".global main\n.global _main\n.text\nmain:\ncall _main\n# move the return value into the first argument for the syscall\nmovq %rax, %rdi\n# move the exit syscall number into rax\nmovq $0x3C, %rax\nsyscall\n_main:"