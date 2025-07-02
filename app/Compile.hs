module Compile
  ( Job (..),
    compile,
  )
where

import Compile.CodeGen (genAsm)
import Compile.Parser (parseAST)
import Compile.Semantic (semanticAnalysis)
import Compile.Translate (translate)
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
  -- let (ir, frameSizes) = translate ast
  -- let asm = genAsm frameSizes ir
  -- liftIO . putStrLn . enumLines $ asm
  -- _ <- liftIO $ readProcess "gcc" ["-x", "assembler", "-", "-o", out job] asm
  return ()

enumLines :: String -> String
enumLines s = unlines $ zipWith (\n l -> show n ++ ": " ++ l) [1 :: Int ..] (lines s)