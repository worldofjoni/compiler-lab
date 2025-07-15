module Compile
  ( Job (..),
    compile,
  )
where

import Compile.CodeGen (genAsm)
import Compile.Dataflow.Liveness (addLiveness)
import Compile.Dataflow.RegAlloc (allocateRegisters)
import Compile.IR (showIRFunc)
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
  liftIO $ putStrLn "compiling..."
  ast <- parseAST $ src job
  liftIO $ putStrLn "parsed"
  semanticAnalysis ast
  liftIO $ putStrLn "semanticed"
  -- liftIO . print $ ast
  let ir = translate ast
  liftIO . putStrLn . unlines . map (showIRFunc . addLiveness) $ ir
  let asm = genAsm (fmap allocateRegisters ir)
  liftIO . putStrLn . enumLines $ asm
  _ <- liftIO $ readProcess "gcc" ["-x", "assembler", "-", "-o", out job] asm
  return ()

enumLines :: String -> String
enumLines s = unlines $ zipWith (\n l -> show n ++ ": " ++ l) [1 :: Int ..] (lines s)