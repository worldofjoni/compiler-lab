{-# OPTIONS_GHC -Wno-unused-imports #-}

module Compile.Semantic
  ( semanticAnalysis,
  )
where

import Compile.AST (AST, Expr (..), Op (..), Simp (Asgn, Decl, Init), Stmt (..), Type (BoolType, IntType), UnOp (BitNot, Neg, Not), posPretty)
import Compile.Parser (parseNumber)
-- Important: Do not remove!

import Control.Applicative (Alternative (some))
import Control.Monad (unless, when)
import qualified Control.Monad.RWS as Map
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import Error (L1ExceptT, semanticFail)
import qualified Text.Megaparsec.Unicode as Map

data VariableStatus
  = Declared
  | Initialized
  deriving (Show)

-- You might want to keep track of some location information as well at some point
type Namespace = Map.Map String (VariableStatus, Type)

type L1Semantic = StateT Namespace L1ExceptT

-- A little wrapper so we don't have to ($ lift) everywhere inside the StateT
semanticFail' :: String -> L1Semantic a
semanticFail' = lift . semanticFail

semanticAnalysis :: AST -> L1ExceptT ()
semanticAnalysis ast = do
  ns <- varStatusAnalysis ast
  evalStateT (checkReturns ast) ns

-- right now an AST is just a list of statements
varStatusAnalysis :: AST -> L1ExceptT Namespace
varStatusAnalysis stmts = do
  execStateT (mapM_ checkStmt stmts) Map.empty

-- So far this checks:
-- + we cannot declare a variable again that has already been declared or initialized
-- + we cannot initialize a variable again that has already been declared or initialized
-- + a variable needs to be declared or initialized before we can assign to it
-- + we can only return valid expressions
checkStmt :: Stmt -> L1Semantic ()
checkStmt (Ret e _) = checkExpr IntType e
checkStmt (SimpStmt s) = checkSimp s
checkStmt (BlockStmt b _) = mapM_ checkStmt b
checkStmt (If i t e _) = do
  checkExpr BoolType i
  checkStmt t
  mapM_ checkStmt e
checkStmt (While c s _) = do
  checkExpr BoolType c
  checkStmt s
checkStmt (For (Just (Init ty name e _)) e' s s' _) = undefined
checkStmt (For (Just (Decl ty name _)) e' s s' _) = undefined
checkStmt (For s1 e' s2 s3 _) = undefined
checkStmt (Break _) = pure ()
checkStmt (Continue _) = pure ()

checkSimp :: Simp -> L1Semantic ()
checkSimp (Decl ty name pos) = do
  ns <- get
  let isDeclared = Map.member name ns
  when isDeclared $
    semanticFail' $
      "Variable " ++ name ++ " redeclared at: " ++ posPretty pos
  put $ Map.insert name (Declared, ty) ns
checkSimp (Init ty name e pos) = do
  ns <- get
  let isDeclared = Map.member name ns
  when isDeclared $
    semanticFail' $
      "Variable " ++ name ++ " redeclared (initialized) at: " ++ posPretty pos
  checkExpr ty e
  put $ Map.insert name (Initialized, ty) ns
checkSimp (Asgn name op e pos) = do
  ns <- get
  case op of
    Nothing -> do
      -- Assignment with `=`
      -- If we assign to a variable with `=`, it has to be either declared or initialized
      let varEntry = Map.lookup name ns
      unless (isJust varEntry) $
        semanticFail' $
          "Trying to assign to undeclared variable "
            ++ name
            ++ " at: "
            ++ posPretty pos
      let ty = snd $ fromJust varEntry
      checkExpr ty e
      put $ Map.insert name (Initialized, ty) ns
    Just _ ->
      -- Assinging with op, e.g. `x += 3`,
      -- for this x needs to be intialized, not just declasred
      case Map.lookup name ns of
        Just (Initialized, IntType) -> do
          checkExpr IntType e
        Just (Initialized, _) -> semanticFail' $ "Expected target variable to be of type int at: " ++ posPretty pos
        _ ->
          semanticFail' $
            "Trying to assignOp to undeclared variable "
              ++ name
              ++ " at: "
              ++ posPretty pos

checkExpr :: Type -> Expr -> L1Semantic ()
checkExpr IntType (IntExpr str pos) = do
  -- Check that literals are in bounds
  let res = parseNumber str
  case res of
    Left e -> do
      semanticFail' $ "Error in " ++ posPretty pos ++ e
    Right _ -> return ()
checkExpr ty (IntExpr _ pos) = semanticFail' $ "Expected " ++ show ty ++ " but got integer at: " ++ posPretty pos
checkExpr ty (IdentExpr name pos) = do
  ns <- get
  case Map.lookup name ns of
    Just (Initialized, ty2) | ty2 == ty -> return ()
    Just (_, ty2) -> semanticFail' $ "Expected " ++ show ty ++ " got " ++ show ty2 ++ " at: " ++ posPretty pos
    _ ->
      semanticFail' $
        "Variable "
          ++ name
          ++ " used without initialization at: "
          ++ posPretty pos
checkExpr IntType (UnExpr Neg e) = checkExpr IntType e
checkExpr IntType (UnExpr BitNot e) = checkExpr IntType e
checkExpr IntType (UnExpr Not e) = checkExpr BoolType e
checkExpr ty (UnExpr op _) = semanticFail' $ show op ++ " does not produce an " ++ show ty
checkExpr BoolType (BinExpr lhs op rhs) = do
  if op `elem` intToBoolOp
    then checkExpr IntType lhs >> checkExpr IntType rhs
    else
      if op `elem` boolToBoolOp
        then checkExpr BoolType lhs >> checkExpr BoolType rhs
        else semanticFail' $ show op ++ " does not produce an bool"
checkExpr IntType (BinExpr lhs op rhs)
  | op `elem` intToIntOp = checkExpr IntType lhs >> checkExpr IntType rhs
  | otherwise = semanticFail' $ "Operator " ++ show op ++ "does not produce an integer."
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

checkReturns :: AST -> L1Semantic ()
checkReturns stmts = do
  let returns = any isReturn stmts
  unless returns $ semanticFail' "Program does not return"
  where
    isReturn (Ret _ _) = True
    isReturn _ = False
