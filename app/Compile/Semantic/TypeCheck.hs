module Compile.Semantic.TypeCheck (varStatusAnalysis) where

import Compile.AST (AST, Expr (..), Op (..), Simp (Asgn, Decl, Init), Stmt (..), Type (BoolType, IntType), UnOp (BitNot, Neg, Not), isDecl, posPretty)
import Compile.Parser (parseNumber)
import Control.Monad (unless, void, when)
import Control.Monad.State
import Control.Monad.Trans.Except (catchE)
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import Error (L1ExceptT, semanticFail)

data VariableStatus
  = Declared
  | Initialized
  deriving (Show, Eq)

-- You might want to keep track of some location information as well at some point
type Namespace = Map.Map String Type

type L1TypeCheck = StateT Namespace L1ExceptT

-- A little wrapper so we don't have to ($ lift) everywhere inside the StateT
semanticFail' :: String -> L1TypeCheck a
semanticFail' = lift . semanticFail

-- right now an AST is just a list of statements
varStatusAnalysis :: AST -> L1ExceptT Namespace
varStatusAnalysis stmts = do
  execStateT (mapM_ checkStmt stmts) Map.empty

subscope :: L1TypeCheck () -> L1TypeCheck ()
subscope action = do
  ns <- get
  void . lift . runStateT action $ ns

declare :: String -> Type -> L1TypeCheck ()
declare name ty = modify (Map.insert name ty)

-- So far this checks:
-- + we cannot declare a variable again that has already been declared or initialized
-- + we cannot initialize a variable again that has already been declared or initialized
-- + a variable needs to be declared or initialized before we can assign to it
-- + we can only return valid expressions
checkStmt :: Stmt -> L1TypeCheck ()
checkStmt (Ret e _) = checkExpr IntType e
checkStmt (SimpStmt s) = checkSimp s
checkStmt (BlockStmt b _) =
  subscope $ mapM_ checkStmt b
checkStmt (If i t e _) = do
  checkExpr BoolType i
  subscope $ checkStmt t
  subscope $ mapM_ checkStmt e
checkStmt (While c s _) = subscope $ do
  checkExpr BoolType c
  checkStmt s
checkStmt (For init_ e' step s' p) = subscope $ do
  traverse_ checkSimp init_
  subscope $ do
    checkExpr BoolType e'
    traverse_ (\step' -> when (isDecl step') $ semanticFail' ("Step statement must not be a declatation at: " ++ posPretty p) >> checkSimp step') step
    checkStmt s'
checkStmt (Break _) = pure ()
checkStmt (Continue _) = pure ()

checkSimp :: Simp -> L1TypeCheck ()
checkSimp (Decl ty name pos) = do
  isDeclared <- gets (Map.member name)
  when isDeclared $
    semanticFail' $
      "Variable " ++ name ++ " redeclared at: " ++ posPretty pos
  declare name ty
checkSimp (Init ty name e pos) = do
  isDeclared <- gets (Map.member name)
  when isDeclared $
    semanticFail' $
      "Variable " ++ name ++ " redeclared (initialized) at: " ++ posPretty pos
  checkExpr ty e
  declare name ty
checkSimp (Asgn name _ e pos) = do
  val <- gets (Map.lookup name)
  case val of
    Nothing ->
      semanticFail' $
        "Trying to assign to undeclared variable "
          ++ name
          ++ " at: "
          ++ posPretty pos
    Just ty ->
      checkExpr ty e

checkExpr :: Type -> Expr -> L1TypeCheck ()
checkExpr IntType (IntExpr str pos) = do
  -- Check that literals are in bounds
  let res = parseNumber str
  case res of
    Left e -> do
      semanticFail' $ "Error in " ++ posPretty pos ++ e
    Right _ -> return ()
checkExpr ty (IntExpr _ pos) = semanticFail' $ "Expected " ++ show ty ++ " but got integer at: " ++ posPretty pos
checkExpr ty (IdentExpr name pos) = do
  val <- gets (Map.lookup name)
  case val of
    Just ty2
      | ty2 == ty -> return ()
      | otherwise -> semanticFail' $ "Expected " ++ show ty ++ " got " ++ show ty2 ++ " at: " ++ posPretty pos
    _ ->
      semanticFail' $
        "Variable "
          ++ name
          ++ " undeclared at: "
          ++ posPretty pos
checkExpr IntType (UnExpr Neg e) = checkExpr IntType e
checkExpr IntType (UnExpr BitNot e) = checkExpr IntType e
checkExpr BoolType (UnExpr Not e) = checkExpr BoolType e
checkExpr ty (UnExpr op _) = semanticFail' $ show op ++ " does not produce an " ++ show ty
checkExpr BoolType (BinExpr lhs op rhs) = do
  ns <- get
  lift $
    catchE
      (int ns)
      ( \e1 ->
          catchE
            (bool ns)
            ( \e2 ->
                if op `elem` boolToBoolOp ++ intToBoolOp
                  then semanticFail $ "Operator " ++ show op ++ " does neigher accept int nor bool:\n" ++ show e1 ++ "\n" ++ show e2
                  else failOp
            )
      )
  where
    int =
      evalStateT $ do
        unless (op `elem` intToBoolOp) $ semanticFail' ""
        checkExpr IntType lhs
        checkExpr IntType rhs
    bool =
      evalStateT $ do
        unless (op `elem` boolToBoolOp) $ semanticFail' ""
        checkExpr BoolType lhs >> checkExpr BoolType rhs
    failOp =
      semanticFail $
        "Operator " ++ show op ++ " does not produce an bool."
checkExpr IntType (BinExpr lhs op rhs)
  | op `elem` intToIntOp = checkExpr IntType lhs >> checkExpr IntType rhs
  | otherwise = semanticFail' $ "Operator " ++ show op ++ " does not produce an integer."
checkExpr BoolType (BoolExpr _ _) = pure ()
checkExpr ty (BoolExpr _ _) = semanticFail' $ "expected " ++ show ty ++ " got bool"
checkExpr ty (Ternary a b c) = do
  checkExpr BoolType a
  checkExpr ty b
  checkExpr ty c

intToBoolOp :: [Op]
intToBoolOp = [Lt, Le, Gt, Ge, Eq, Neq]

boolToBoolOp :: [Op]
boolToBoolOp = [And, Or, Eq, Neq]

intToIntOp :: [Op]
intToIntOp = [Add, Sub, Mul, Div, Mod, Shl, Shr, BitOr, BitAnd, BitXor]