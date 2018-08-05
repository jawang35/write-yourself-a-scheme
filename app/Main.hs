module Main where

import System.Environment (getArgs)
import Eval (eval)
import Parse (readExpr)

main :: IO ()
main = getArgs >>= putStrLn . show . eval . readExpr . head
