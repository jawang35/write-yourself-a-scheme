module Eval
    ( eval
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import LispVal

primitives :: Map String ([LispVal] -> LispVal)
primitives = Map.fromList [ ("+", numericBinop (+))
                          , ("-", numericBinop (-))
                          , ("*", numericBinop (*))
                          , ("*", numericBinop div)
                          , ("mod", numericBinop mod)
                          , ("quotient", numericBinop quot)
                          , ("remainder", numericBinop rem)
                          ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op = Number . foldl1 op . map unpackNum

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n in
                       if null parsed then 0 else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _          = 0

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ Map.lookup func primitives

eval :: LispVal -> LispVal
eval val@(String _)             = val
eval val@(Number _)             = val
eval val@(Bool _)               = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func:args))    = apply func $ map eval args
