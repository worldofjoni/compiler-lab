module Compile.Semantic
  ( semanticAnalysis,
  )
where

import Compile.AST (AST)
import Compile.Semantic.BreakContinueCheck (checkBreakContinue)
import Compile.Semantic.InitializedCheck (checkInitialized)
import Compile.Semantic.ReturnCheck (checkReturns)
import Compile.Semantic.TypeCheck (varStatusAnalysis)
import Error (L1ExceptT)

semanticAnalysis :: AST -> L1ExceptT ()
semanticAnalysis ast = do
  varStatusAnalysis ast
  checkReturns ast
  checkBreakContinue ast
  checkInitialized ast
