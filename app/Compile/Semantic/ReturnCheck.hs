module Compile.Semantic.ReturnCheck (checkReturns) where

import Compile.AST (AST, Stmt (Ret))
import Control.Monad (unless)
import Error (L1ExceptT, semanticFail)

checkReturns :: AST -> L1ExceptT ()
checkReturns stmts = do
  let returns = any isReturn stmts
  unless returns $ semanticFail "Program does not return"
  where
    isReturn (Ret _ _) = True
    isReturn _ = False
