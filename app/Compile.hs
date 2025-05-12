module Compile
  ( Job (..),
    compile,
  )
where

import Compile.AsmGen (genAsm)
import Compile.CodeGen (codeGen)
import Compile.Parser (parseAST)
import Compile.Semantic (semanticAnalysis)
import Control.Monad.IO.Class
import Error (L1ExceptT)
import System.Process (readProcess)

data Job = Job
  { src :: FilePath,
    out :: FilePath
  }
  deriving (Show)

compile :: Job -> L1ExceptT ()
compile job = do
  ast <- parseAST $ src job
  semanticAnalysis ast
  let ir = codeGen ast
  let asm = genAsm 0 ir
  liftIO $ putStrLn asm
  _ <- liftIO $ readProcess "gcc" ["-x", "assembler", "-", "-o", out job] asm
  -- liftIO $ writeFile (out job) asm
  return ()
