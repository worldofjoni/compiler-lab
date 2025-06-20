module Compile.Semantic.BreakContinueCheck where

import Compile.AST 
import Control.Monad (unless)
import Error (L1ExceptT, semanticFail)
import Data.Maybe (maybeToList)

checkBreakContinue :: AST -> L1ExceptT ()
checkBreakContinue = mapM_ (check 0) . concatMap functionBody

functionBody :: Function -> Block
functionBody (Func _ _ _ body _) = body

check:: Int -> Stmt -> L1ExceptT ()
check loops (Break pos) = unless (loops > 0) $ semanticFail $ "break outside loop at " ++ posPretty pos
check loops (Continue pos) = unless (loops > 0) $ semanticFail $ "continue outside loop at " ++ posPretty pos
check loops (While _ body _) = check (loops + 1) body
check loops (For _ _ _ body _) = check (loops + 1) body
check loops (BlockStmt stmts _) = mapM_ (check loops) stmts
check loops (If _ thenStmt elseStmt _) = mapM_ (check loops) (thenStmt:maybeToList elseStmt)
check _ _ = pure ()