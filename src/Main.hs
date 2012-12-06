module Main where

import System.Environment
import Text.PrettyPrint.HughesPJ

import Language.MIXAL.Parser
import Language.MIXAL.PP (ppMIXALStmt)

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
    Right is -> do
           mapM_ print is
           mapM_ (putStrLn . render . ppMIXALStmt) is
