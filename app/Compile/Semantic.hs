module Compile.Semantic
  ( semanticAnalysis,
  )
where

import Compile.AST (AST)
import Compile.Semantic.ReturnCheck (checkReturns)
import Compile.Semantic.TypeCheck (varStatusAnalysis)
import Error (L1ExceptT)

semanticAnalysis :: AST -> L1ExceptT ()
semanticAnalysis ast = do
  _ <- varStatusAnalysis ast
  checkReturns ast
