module Lib
    ( readExpr
    ) where

import Data.Char (digitToInt)
import Numeric (readInt, readHex, readOct)
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where
    show (String contents) = "\"" ++ contents ++ "\""
    show (Atom name) = name
    show (Number contents) = show contents
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List contents) = "(" ++ unwordsList contents ++ ")"
    show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"

unwordsList :: (Show a) => [a] -> String
unwordsList = unwords . map show

symbol :: Parser Char
symbol = oneOf "!$%&!*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do
    first <- choice [letter, symbol]
    rest <- many $ choice [letter, digit, symbol]
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = let numberParsers = map (\(prefix, reader, rawParser) -> try (string prefix) >> (Number . reader <$> many1 rawParser)) numberTypes in
    choice $ (Number . read <$> many1 digit):numberParsers
    where numberTypes = [ ("#d", read, digit)
                        , ("#b", fst . head . readInt 2 (`elem` "01") digitToInt, oneOf "01")
                        , ("#o", fst . head . readOct, octDigit)
                        , ("#x", fst . head . readHex, hexDigit)
                        ]

escapeChars = choice (map (\(code, replacement) -> try (string code) >> return replacement) codeReplacements)
    where codeReplacements = [ ("\\\"", '"')
                             , ("\\n", '\n')
                             , ("\\r", '\r')
                             , ("\\t", '\t')
                             , ("\\\\", '\\')
                             ]

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ choice [escapeChars, noneOf "\""]
    char '"'
    return $ String x

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseLists = do
    char '('
    x <- choice [try parseList, parseDottedList]
    char ')'
    return x

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = choice [parseQuoted, parseNumber, parseAtom, parseString, parseLists]

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val
