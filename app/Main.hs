module Main where

import Control.Monad (liftM)
import System.Environment (getArgs)
import Lisp (eval, extractValue, readExpr, trapError)

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (head args) >>= eval
    putStrLn $ extractValue $ trapError evaled
