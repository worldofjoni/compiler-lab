module Compile.Semantic
  ( semanticAnalysis,
  )
where

import Compile.AST (AST)
import Compile.Semantic.BreakContinueCheck (checkBreakContinue)
import Compile.Semantic.InitializedCheck (checkInitialized)
import Compile.Semantic.ReturnCheck (checkReturns)
import Compile.Semantic.TypeCheck (StructDefs, varStatusAnalysis)
import Error (L1ExceptT)

semanticAnalysis :: AST -> L1ExceptT StructDefs
semanticAnalysis ast = do
  structDefs <- varStatusAnalysis ast
  checkReturns ast
  checkBreakContinue ast
  checkInitialized ast
  pure structDefs
