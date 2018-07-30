module Main where

import System.Environment (getArgs)
import Lib (readExpr)

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ readExpr $ head args
