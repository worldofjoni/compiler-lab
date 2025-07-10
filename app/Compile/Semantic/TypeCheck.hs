module Compile.Semantic.TypeCheck (varStatusAnalysis) where

import Compile.AST
import Compile.Parser (parseNumber)
import Control.Monad (unless, void, when, zipWithM_)
import Control.Monad.State
import Control.Monad.Trans.Except (catchE)
import Data.Foldable (traverse_)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Error (L1ExceptT, semanticFail)
import Text.Megaparsec (SourcePos, sourcePosPretty)

data VariableStatus
  = Declared
  | Initialized
  deriving (Show, Eq)

type StructDefs = Map.Map Ident (Map.Map Ident Type)

-- You might want to keep track of some location information as well at some point
type Namespace = Map.Map String Type

type Signature = (Type, [Type])

functionSignatures :: [Function] -> L1ExceptT (Map.Map String Signature)
functionSignatures ast = do
  let sigs = map sig ast ++ predefined
  let names = map fst sigs
  let sigMap = Map.fromList $ sigs
  mapM_ checkParams ast
  unless (distinct names) (semanticFail "function names are not distinct")
  unless ("main" `elem` names) (semanticFail "no main function")
  unless (fromJust (Map.lookup "main" sigMap) == (IntType, [])) (semanticFail "main must return int and have no parameters")
  return sigMap
  where
    sig (Func ret name params _ _) = (name, (ret, map fst params))
    predefined = [("print", (IntType, [IntType])), ("read", (IntType, [])), ("flush", (IntType, []))]

checkParams :: Function -> L1ExceptT ()
checkParams (Func ret name params _ pos) = do
  unless (distinct . map snd $ params) (semanticFail $ "parameters of function " ++ name ++ " do not have distinct names at " ++ sourcePosPretty pos)
  unless (isSmall ret) . semanticFail $ "Function return types need to be small, but `" ++ show ret ++ "` is not at " ++ posPretty pos
  mapM_ (\(ty, _) -> unless (isSmall ty) . semanticFail $ "Function parameters need to be small types, but " ++ show ty ++ " is not at " ++ posPretty pos) params

distinct :: [String] -> Bool
distinct l = and $ zipWith (/=) sorted (tail sorted)
  where
    sorted = sort l

type L1TypeCheck = StateT Context L1ExceptT

data Context = Context
  { namespace :: Namespace,
    signature :: Map.Map String Signature,
    structs :: StructDefs,
    currRetType :: Type
  }

-- A little wrapper so we don't have to ($ lift) everywhere inside the StateT
semanticFail' :: String -> L1TypeCheck a
semanticFail' = lift . semanticFail

initalNamespace :: [(Type, String)] -> Namespace
initalNamespace = Map.fromList . map swap

checkStructDefs :: StructDefs -> AST -> L1ExceptT StructDefs
checkStructDefs defs [] = pure defs
checkStructDefs defs (Function _ : as) = checkStructDefs defs as
checkStructDefs defs (Struct (StructDef name fields) : as) = do
  when (Map.member name defs) $ semanticFail $ "Struct " ++ name ++ " already defined."
  when (length fields /= Set.size (Set.fromList fields)) $ semanticFail $ "Struct " ++ name ++ " contains duplicate fields."
  let defs' = Map.insert name (Map.fromList $ map swap fields) defs
  checkStructDefs defs' as

varStatusAnalysis :: AST -> L1ExceptT ()
varStatusAnalysis ast = do
  structDefs <- checkStructDefs Map.empty ast
  signatures <- functionSignatures fs
  mapM_ (checkFunction signatures structDefs) fs
  where
    fs = filterFunctions ast

checkFunction :: Map.Map String Signature -> StructDefs -> Function -> L1ExceptT ()
checkFunction signatrues structDefs (Func t _ params stmts _) = do
  void $ execStateT (mapM_ checkStmt stmts) initialState
  where
    initialState = Context (initalNamespace params) signatrues structDefs t

subscope :: L1TypeCheck () -> L1TypeCheck ()
subscope action = do
  ns <- get
  void . lift . runStateT action $ ns

declare :: String -> Type -> L1TypeCheck ()
declare name ty =
  modify $ \s -> s {namespace = Map.insert name ty (namespace s)}

-- So far this checks:
-- + we cannot declare a variable again that has already been declared or initialized
-- + we cannot initialize a variable again that has already been declared or initialized
-- + a variable needs to be declared or initialized before we can assign to it
-- + we can only return valid expressions
-- + function calls
checkStmt :: Stmt -> L1TypeCheck ()
checkStmt (Ret e _) = do
  t <- gets currRetType
  checkExpr t e
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
  checkExpr BoolType e'
  traverse_
    ( \step' -> do
        when (isDecl step') $ semanticFail' ("Step statement must not be a declatation at: " ++ posPretty p)
        checkSimp step'
    )
    step
  checkStmt s'
checkStmt (Break _) = pure ()
checkStmt (Continue _) = pure ()

checkCall :: Maybe Type -> String -> [Expr] -> SourcePos -> L1TypeCheck ()
checkCall mRetType name args pos = do
  m <- gets (Map.lookup name . signature)
  case m of
    Nothing -> semanticFail' $ "call of undefined function " ++ name ++ " at " ++ sourcePosPretty pos
    Just (retT, types) -> do
      unless (length types == length args) (semanticFail' ("expected " ++ show (length types) ++ " arguments, but got " ++ show (length args) ++ " at " ++ sourcePosPretty pos))
      zipWithM_ checkExpr types args
      unless (fromMaybe retT mRetType == retT) (semanticFail' (name ++ " returns " ++ show retT ++ " but " ++ show (fromJust mRetType) ++ " was expected at " ++ sourcePosPretty pos))

checkSimp :: Simp -> L1TypeCheck ()
checkSimp (Decl ty name pos) = do
  unless (isSmall ty) . semanticFail' $ "Local variables need to be small types, but " ++ show ty ++ " is not at " ++ posPretty pos
  isDeclared <- gets (Map.member name . namespace)
  when isDeclared $
    semanticFail' $
      "Variable " ++ name ++ " redeclared at: " ++ posPretty pos
  declare name ty
checkSimp (Init ty name e pos) = do
  unless (isSmall ty) . semanticFail' $ "Local variables need to be small types, but " ++ show ty ++ " is not at " ++ posPretty pos
  isDeclared <- gets (Map.member name . namespace)
  when isDeclared $
    semanticFail' $
      "Variable " ++ name ++ " redeclared (initialized) at: " ++ posPretty pos
  checkExpr ty e
  declare name ty
checkSimp (Asgn lv Nothing e pos) = do
  ty <- lvalueType lv pos
  unless (isSmall ty) . semanticFail' $ "Lvalue needs to be small type, but " ++ show ty ++ " is large at " ++ posPretty pos
  checkExpr ty e
checkSimp (Asgn lv (Just op) e pos) = do
  ty <- lvalueType lv pos
  unless (isSmall ty) . semanticFail' $ "Lvalue needs to be small type, but " ++ show ty ++ " is large at " ++ posPretty pos
  case ty of
    IntType -> checkExpr IntType e
    _ -> semanticFail' $ show op ++ " does not work on " ++ show ty ++ ", only int at: " ++ posPretty pos
checkSimp (SimpCall name args pos) = checkCall Nothing name args pos

lvalueType :: LValue -> SourcePos -> L1TypeCheck Type
lvalueType (Var name) pos = do
  val <- gets (Map.lookup name . namespace)
  case val of
    Nothing -> undeclaredFail name pos
    Just ty -> pure ty
lvalueType (Deref lv) pos = do
  pt <- lvalueType lv pos
  case pt of
    PointerType t -> pure t
    _ -> semanticFail' $ "Cannot dereference pointer at " ++ posPretty pos
lvalueType (ArrayAccess lv e) pos = do
  at <- lvalueType lv pos
  case at of
    ArrayType t -> checkExpr IntType e >> pure t
    wt -> semanticFail' $ show wt ++ " is not an array at " ++ posPretty pos
lvalueType (Field lv f) pos = do
  st <- lvalueType lv pos
  case st of
    (StructType t) -> lookupStruct t f
    wt -> semanticFail' $ show wt ++ " is not a struct at " ++ posPretty pos

lookupStruct :: Ident -> Ident -> L1TypeCheck Type
lookupStruct name field = do
  record <- gets (Map.lookup name . structs)
  record' <- maybe (semanticFail' $ "Struct " ++ name ++ " does not exist.") pure record
  maybe (semanticFail' $ "Field " ++ field ++ " does not exists for struct " ++ name) pure (Map.lookup field record')

undeclaredFail :: String -> SourcePos -> L1TypeCheck a
undeclaredFail name pos =
  semanticFail' $
    "Variable "
      ++ name
      ++ " undeclared at: "
      ++ posPretty pos

checkExpr :: Type -> Expr -> L1TypeCheck ()
checkExpr IntType (IntExpr str pos) = do
  -- Check that literals are in bounds
  let res = parseNumber str
  case res of
    Left e -> do
      semanticFail' $ "Error in " ++ posPretty pos ++ e
    Right _ -> return ()
checkExpr ty (IntExpr _ pos) = semanticFail' $ "Expected " ++ show ty ++ " but got integer at: " ++ posPretty pos
checkExpr ty (LValueExpr lv pos) = do
  ty2 <- lvalueType lv pos
  if ty2 == ty
    then return ()
    else
      semanticFail' $ "Expected " ++ show ty ++ " got " ++ show ty2 ++ " at: " ++ posPretty pos
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
                catchE
                  (ptr ns)
                  ( \e3 ->
                      if op `elem` boolToBoolOp ++ intToBoolOp ++ ptrToBoolOp
                        then semanticFail $ "Operator " ++ show op ++ " requieres two bools, ints, or compatible pointers:\n" ++ show e1 ++ "\n" ++ show e2 ++ "\n" ++ show e3
                        else failOp
                  )
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
    ptr = evalStateT $ do
      unless (op `elem` ptrToBoolOp) $ semanticFail' ""
      comp <- isCompatiplePointerExpr lhs rhs
      unless comp $ semanticFail' ""
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
checkExpr ty (Call name args pos) = checkCall (Just ty) name args pos
checkExpr (PointerType _) (Null _) = pure ()
checkExpr t (Null _) = semanticFail' $ "expected pointer type, got " ++ show t
checkExpr (PointerType t) (Alloc u)
  | t == u = pure ()
  | otherwise = semanticFail' $ "pointer to " ++ show t ++ " cannot store " ++ show u
checkExpr (ArrayType t) (AllocArray u e)
  | t == u = checkExpr IntType e
  | otherwise = semanticFail' $ "array of " ++ show t ++ " cannot store " ++ show u
checkExpr (StructType _) _ = error "there are no expressions of struct type, fix your compiler!"
checkExpr t (Alloc _) = semanticFail' $ "Cannot allocate into " ++ show t
checkExpr t (AllocArray _ _) = semanticFail' $ "Cannot allocate array into " ++ show t ++ ": not an array"
checkExpr (PointerType _) _ = semanticFail' "no pointer arithmetric"
checkExpr (ArrayType _) _ = semanticFail' "no array arithmetric"

isCompatiplePointerExpr :: Expr -> Expr -> L1TypeCheck Bool
isCompatiplePointerExpr (Null _) (Null _) = pure True
isCompatiplePointerExpr e n@(Null _) = isCompatiplePointerExpr n e
isCompatiplePointerExpr (Null _) e = isJust <$> isPointerTo e
isCompatiplePointerExpr e1 e2 = do
  p1 <- isPointerTo e1
  p2 <- isPointerTo e2
  case p1 of 
    Nothing -> pure False
    Just _ -> pure (p1 == p2)

-- todo

-- may panic for some expressions
isPointerTo :: Expr -> L1TypeCheck (Maybe Type)
isPointerTo (LValueExpr lv pos) = do
  ty <- lvalueType lv pos
  pure (isPointerTo' ty)
isPointerTo (Ternary _ a _b) = isPointerTo a
isPointerTo (Call name _ _) = do
  ty <- getFunctionReturnType name
  pure $ isPointerTo' ty
isPointerTo (Alloc ty) = pure (Just ty)
isPointerTo (Null _) = error "cannt call on that!"
isPointerTo _ = pure Nothing

isPointerTo' :: Type -> Maybe Type
isPointerTo' (PointerType t) = Just t
isPointerTo' _ = Nothing

getFunctionReturnType :: String -> L1TypeCheck Type
getFunctionReturnType name = do
  gets (fst . fromJust . Map.lookup name . signature)

intToBoolOp :: [Op]
intToBoolOp = [Lt, Le, Gt, Ge, Eq, Neq]

boolToBoolOp :: [Op]
boolToBoolOp = [And, Or, Eq, Neq]

ptrToBoolOp :: [Op]
ptrToBoolOp = [Eq, Neq]

intToIntOp :: [Op]
intToIntOp = [Add, Sub, Mul, Div, Mod, Shl, Shr, BitOr, BitAnd, BitXor]