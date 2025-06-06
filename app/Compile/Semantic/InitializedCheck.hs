module Compile.Semantic.InitializedCheck where

import Compile.AST
import Control.Monad (unless, void)
import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Set as Set
import Error (L1ExceptT, semanticFail)

type Initialized = Set String

data InitState = InitState
  { initialized :: Initialized,
    allInit :: Bool,
    step :: Maybe Simp
  }

type L1InitCheck = StateT InitState L1ExceptT

define :: String -> L1InitCheck ()
define name = modify (\r -> r {initialized = Set.insert name . initialized $ r})

defineFrom :: InitState -> L1InitCheck ()
defineFrom is = modify (\r -> r {allInit = allInit is, initialized = Set.union (initialized is) (initialized r)})

defineAll :: L1InitCheck ()
defineAll = modify (\r -> r {allInit = True})

intersectState :: InitState -> InitState -> InitState
intersectState (InitState i1 r1 e1) (InitState i2 r2 _) =
  InitState (Set.intersection i1 i2) (r1 && r2) e1

checkDefined :: String -> L1InitCheck Bool
checkDefined name = do
  allInitialized <- gets allInit
  if allInitialized then pure True else gets (Set.member name . initialized)

subscope :: L1InitCheck () -> L1InitCheck InitState
subscope action = do
  s <- get
  lift . execStateT action $ s

loopScope :: Simp -> L1InitCheck () -> L1InitCheck ()
loopScope e action = do
  s <- get
  void . lift . execStateT action $ (s {step = Just e})

checkLoopSimp :: L1InitCheck ()
checkLoopSimp = do
  e <- gets step
  traverse_ checkSimp e

-- A little wrapper so we don't have to ($ lift) everywhere inside the StateT
semanticFail' :: String -> L1InitCheck a
semanticFail' = lift . semanticFail

checkInitialized :: AST -> L1ExceptT ()
checkInitialized ast = void $ execStateT (mapM_ checkStmt ast) $ InitState {initialized = Set.empty, allInit = False, step = Nothing}

checkStmt :: Stmt -> L1InitCheck ()
checkStmt (SimpStmt s) = checkSimp s
checkStmt (BlockStmt b _) = mapM_ checkStmt b
checkStmt (If e a (Just b) _) = do
  checkExpr e
  as <- subscope $ checkStmt a
  bs <- subscope $ checkStmt b
  defineFrom (intersectState as bs)
checkStmt (If e a Nothing _) = do
  checkExpr e
  void . subscope $ checkStmt a
checkStmt (While e s _) = do
  checkExpr e
  void . subscope . checkStmt $ s
checkStmt (For a b (Just q) body _) = do
  traverse_ checkSimp a
  loopScope q $ do
    checkExpr b
    checkStmt body
    checkLoopSimp
checkStmt (For a b Nothing body _) = do
  traverse_ checkSimp a
  void . subscope $ do
    checkExpr b
    checkStmt body
    checkLoopSimp
checkStmt (Break {}) = checkLoopSimp >> defineAll
checkStmt (Continue {}) = checkLoopSimp >> defineAll
checkStmt (Ret e _) = checkExpr e >> defineAll

checkSimp :: Simp -> L1InitCheck ()
checkSimp (Decl {}) = pure ()
checkSimp (Init _ name e _) = checkExpr e >> define name
checkSimp (Asgn target Nothing expr _) = checkExpr expr >> define target
checkSimp (Asgn target (Just _) expr _) = checkDefined target >> checkExpr expr

checkExpr :: Expr -> L1InitCheck ()
checkExpr (IntExpr _ _) = pure ()
checkExpr (BoolExpr _ _) = pure ()
checkExpr (IdentExpr name pos) = do
  defined <- checkDefined name
  unless defined $ semanticFail' $ "Variable " ++ name ++ " is uninitialized at " ++ posPretty pos
checkExpr (BinExpr e1 _ e2) = checkExpr e1 >> checkExpr e2
checkExpr (UnExpr _ e) = checkExpr e
checkExpr (Ternary a b c) = checkExpr a >> checkExpr b >> checkExpr c