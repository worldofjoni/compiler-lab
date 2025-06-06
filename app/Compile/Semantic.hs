module Compile.Semantic
  ( semanticAnalysis,
  )
where

import Compile.AST (AST)
import Compile.Semantic.ReturnCheck (checkReturns)
import Compile.Semantic.TypeCheck (varStatusAnalysis)
import Error (L1ExceptT)
import Compile.Semantic.BreakContinueCheck (checkBreakContinue)
import Control.Monad (void)

semanticAnalysis :: AST -> L1ExceptT ()
semanticAnalysis ast = do
  checkReturns ast
  checkBreakContinue ast
  void $ varStatusAnalysis ast
