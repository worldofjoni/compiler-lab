module Compile
  ( Job (..),
    compile,
  )
where

import Compile.Translate (translate)
import Compile.CodeGen (genAsm)
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
  let ir = translate ast
  let asm = genAsm 0 ir
  liftIO . putStrLn . enumLines $ asm
  _ <- liftIO $ readProcess "gcc" ["-x", "assembler", "-", "-o", out job] asm
  return ()


enumLines :: String -> String
enumLines s = unlines $ zipWith (\n l -> show n ++ ": " ++ l) [1 :: Int ..] (lines s)