module Main where

import System.Environment
import Text.PrettyPrint.HughesPJ

import System.MIX.MIXALParser
import System.MIX.PP

main :: IO ()
main = do
  args <- getArgs
  pName <- getProgName
  if length args /= 1 then
      error $ "Usage: " ++ pName ++ " <filename>" else
      return ()

  let [fname] = args

  s <- readFile fname
  let r = parseMIXAL fname s
  case r of
    Left e -> putStrLn $ "Error: " ++ show e
    Right is ->
        mapM_ (putStrLn . render . ppMIXALStmt) is
