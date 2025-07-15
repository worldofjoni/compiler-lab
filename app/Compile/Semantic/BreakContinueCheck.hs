module Compile.Semantic.BreakContinueCheck where

import Compile.AST
import Control.Monad (unless)
import Data.Maybe (maybeToList)
import Error (L1ExceptT, semanticFail)

checkBreakContinue :: AST -> L1ExceptT ()
checkBreakContinue = mapM_ (check 0) . concatMap functionBody

functionBody :: Definition -> Block
functionBody (Function (Func _ _ _ body _)) = body
functionBody _ = []

check :: Int -> Stmt -> L1ExceptT ()
check loops (Break pos) = unless (loops > 0) $ semanticFail $ "break outside loop at " ++ posPretty pos
check loops (Continue pos) = unless (loops > 0) $ semanticFail $ "continue outside loop at " ++ posPretty pos
check loops (While _ body _) = check (loops + 1) body
check loops (For _ _ _ body _) = check (loops + 1) body
check loops (BlockStmt stmts _) = mapM_ (check loops) stmts
check loops (If _ thenStmt elseStmt _) = mapM_ (check loops) (thenStmt : maybeToList elseStmt)
check _ _ = pure ()