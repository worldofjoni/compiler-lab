module Main where

import Args (jobParser, validateJob)
import Compile (compile)
import Control.Monad.Except (runExceptT)
import Error (dieWithError)
import Options.Applicative (execParser)

main :: IO ()
main = do
  input <- execParser jobParser
  result <-
    runExceptT $ do
      job <- validateJob input
      compile job
  case result of
    Left err -> dieWithError err
    Right () -> return ()
