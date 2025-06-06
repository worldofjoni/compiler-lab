module Compile.Semantic.ReturnCheck (checkReturns) where

import Compile.AST 
import Control.Monad (unless)
import Error (L1ExceptT, semanticFail)

checkReturns :: AST -> L1ExceptT ()
checkReturns stmts = do
  unless (any returns stmts) $ semanticFail "Program does not return"

returns:: Stmt -> Bool
returns (Ret _ _) = True
returns (If _ thenStmt (Just elseStmt) _) = returns thenStmt && returns elseStmt
returns (BlockStmt (x:xs) sourcePos) = returns x || returns (BlockStmt xs sourcePos)
returns _ = False
