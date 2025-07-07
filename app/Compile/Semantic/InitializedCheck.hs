module Compile.Semantic.InitializedCheck where

import Compile.AST
import Control.Monad (unless, void)
import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Set as Set
  ( Set,
    empty,
    fromList,
    insert,
    intersection,
    member,
    union,
    (\\),
  )
import Error (L1ExceptT, semanticFail)
import Text.Megaparsec (SourcePos)

type Initialized = Set String

data InitState = InitState
  { scopes :: [Set String],
    initialized :: Initialized,
    step :: Maybe Simp
  }

newState :: [(Type, String)] -> InitState
newState parms = InitState {initialized = Set.fromList . map snd $ parms, step = Nothing, scopes = [Set.empty]}

type L1InitCheck = StateT InitState L1ExceptT

define :: String -> L1InitCheck ()
define name = modify (\r -> r {initialized = Set.insert name . initialized $ r})

defineFrom :: Initialized -> L1InitCheck ()
defineFrom is = modify (\r -> r {initialized = Set.union is (initialized r)})

defineAll :: L1InitCheck ()
defineAll = modify (\r -> r {initialized = foldl Set.union Set.empty (scopes r)})

assertDefined :: String -> SourcePos -> L1InitCheck ()
assertDefined name pos = do
  defined <- gets (Set.member name . initialized)
  unless defined $ semanticFail' $ "Variable " ++ name ++ " is uninitialized at " ++ posPretty pos

tryInitialized :: L1InitCheck () -> L1InitCheck Initialized
tryInitialized action = do
  s <- get
  initialized <$> (lift . execStateT action $ s)

declare :: String -> L1InitCheck ()
declare name = modify (\r -> r {scopes = update (scopes r)})
  where
    update (s : ss) = Set.insert name s : ss
    update [] = error "no scope yet.."

scope :: L1InitCheck () -> L1InitCheck ()
scope action = do
  modify (\r -> r {scopes = Set.empty : scopes r})
  action
  s <- gets (head . scopes)
  modify (\r -> r {initialized = initialized r \\ s, scopes = tail (scopes r)})

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
checkInitialized [] = pure ()
checkInitialized (Function (Func _ _ params stmts _) : fs) = do
  _ <- execStateT (mapM_ checkStmt stmts) (newState params)
  checkInitialized fs
checkInitialized (_ : fs) = checkInitialized fs

checkStmt :: Stmt -> L1InitCheck ()
checkStmt (SimpStmt s) = checkSimp s
checkStmt (BlockStmt b _) = scope $ mapM_ checkStmt b
checkStmt (If e a (Just b) _) = do
  checkExpr e
  as <- tryInitialized . scope . checkStmt $ a
  bs <- tryInitialized . scope . checkStmt $ b
  defineFrom (Set.intersection as bs)
checkStmt (If e a Nothing _) = do
  scope $ checkExpr e
  void . tryInitialized $ checkStmt a
checkStmt (While e s _) = do
  checkExpr e
  scope . void . tryInitialized . checkStmt $ s
checkStmt (For a b (Just q) body _) = scope $ do
  traverse_ checkSimp a
  loopScope q $ do
    checkExpr b
    checkStmt body
    checkLoopSimp
checkStmt (For a b Nothing body _) = scope $ do
  traverse_ checkSimp a
  void . tryInitialized $ do
    checkExpr b
    checkStmt body
    checkLoopSimp
checkStmt (Break {}) = checkLoopSimp >> defineAll
checkStmt (Continue {}) = checkLoopSimp >> defineAll
checkStmt (Ret e _) = checkExpr e >> defineAll

checkSimp :: Simp -> L1InitCheck ()
checkSimp (Decl _ name _) = declare name
checkSimp (Init _ name e _) = checkExpr e >> declare name >> define name
checkSimp (Asgn (Var target) Nothing expr _) = checkExpr expr >> define target
checkSimp (Asgn (Var target) (Just _) expr pos) = assertDefined target pos >> checkExpr expr
checkSimp (SimpCall _ args _) = mapM_ checkExpr args

checkExpr :: Expr -> L1InitCheck ()
checkExpr (IntExpr _ _) = pure ()
checkExpr (BoolExpr _ _) = pure ()
checkExpr (LValueExpr (Var name) pos) = assertDefined name pos
checkExpr (BinExpr e1 _ e2) = checkExpr e1 >> checkExpr e2
checkExpr (UnExpr _ e) = checkExpr e
checkExpr (Ternary a b c) = checkExpr a >> checkExpr b >> checkExpr c
checkExpr (Call _ args _) = mapM_ checkExpr args