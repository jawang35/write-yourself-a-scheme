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
                          , ("boolean?", testBoolean)
                          , ("list?", testList)
                          , ("symbol?", testSymbol)
                          , ("number?", testNumber)
                          , ("string?", testString)
                          ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op = Number . foldl1 op . map unpackNum

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _          = 0

testBoolean :: [LispVal] -> LispVal
testBoolean [Bool _] = Bool True
testBoolean _        = Bool False

testList :: [LispVal] -> LispVal
testList [List _] = Bool True
testList _        = Bool False

testSymbol :: [LispVal] -> LispVal
testSymbol [Atom _] = Bool True
testSymbol _        = Bool False

testNumber :: [LispVal] -> LispVal
testNumber [Number _] = Bool True
testNumber _          = Bool False

testString :: [LispVal] -> LispVal
testString [String _] = Bool True
testString _          = Bool False

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ Map.lookup func primitives

eval :: LispVal -> LispVal
eval (List [Atom "quote", val]) = val
eval (List (Atom func:args))    = apply func $ map eval args
eval val                        = val
