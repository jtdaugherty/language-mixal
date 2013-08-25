module Main where

import Control.Monad (when)
import System.Environment
import Text.PrettyPrint.HughesPJ

import Language.MIXAL.Parser
import Language.MIXAL.PP (ppMIXALStmt)

main :: IO ()
main = do
  args <- getArgs
  pName <- getProgName

  when (length args /= 1) $
       error $ "Usage: " ++ pName ++ " <filename>"

  let [fname] = args

  s <- readFile fname
  case parseMIXAL fname s of
    Left e -> putStrLn $ "Error: " ++ show e
    Right is -> do
         putStrLn "AST:"
         mapM_ print is

         putStrLn "\nPretty-printed source:"
         mapM_ (putStrLn . render . ppMIXALStmt) is
