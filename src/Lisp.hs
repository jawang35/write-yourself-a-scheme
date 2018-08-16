module Lisp
    ( Env
    , eval
    , extractValue
    , liftThrows
    , nullEnv
    , readExpr
    , runIOThrows
    , trapError
    ) where

import Control.Monad.Except (ExceptT, catchError, liftIO, runExceptT, throwError)
import Data.Char (digitToInt)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Numeric (readInt, readHex, readOct)
import Text.ParserCombinators.Parsec hiding (spaces)

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

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

primitives :: Map String ([LispVal] -> ThrowsError LispVal)
primitives = Map.fromList [ ("+", numericBinop (+))
                          , ("-", numericBinop (-))
                          , ("*", numericBinop (*))
                          , ("*", numericBinop div)
                          , ("mod", numericBinop mod)
                          , ("quotient", numericBinop quot)
                          , ("remainder", numericBinop rem)
                          , ("=", numBoolBinop (==))
                          , ("<", numBoolBinop (<))
                          , (">", numBoolBinop (>))
                          , ("/=", numBoolBinop (/=))
                          , ("<=", numBoolBinop (<=))
                          , (">=", numBoolBinop (>=))
                          , ("&&", boolBoolBinop (&&))
                          , ("||", boolBoolBinop (||))
                          , ("string=?", strBoolBinop (==))
                          , ("string<?", strBoolBinop (<))
                          , ("string<=?", strBoolBinop (<=))
                          , ("string>=?", strBoolBinop (>=))
                          , ("car", car)
                          , ("cdr", cdr)
                          , ("cons", cons)
                          , ("eq?", eqv)
                          , ("eqv?", eqv)
                          , ("boolean?", testBoolean)
                          , ("list?", testList)
                          , ("symbol?", testSymbol)
                          , ("number?", testNumber)
                          , ("string?", testString)
                          , ("symbol->string", symbolToString)
                          , ("string->symbol", stringToSymbol)
                          ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = Number . foldl1 op <$> mapM unpackNum params

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op [x, y] = do
    left <- unpacker x
    right <- unpacker y
    return $ Bool $ left `op` right
boolBinop _ _ args = throwError $ NumArgs 2 args

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

testBoolean :: [LispVal] -> ThrowsError LispVal
testBoolean []       = throwError $ NumArgs 1 []
testBoolean [Bool _] = return $ Bool True
testBoolean _        = return $ Bool False

testList :: [LispVal] -> ThrowsError LispVal
testList []       = throwError $ NumArgs 1 []
testList [List _] = return $ Bool True
testList _        = return $ Bool False

testSymbol :: [LispVal] -> ThrowsError LispVal
testSymbol []       = throwError $ NumArgs 1 []
testSymbol [Atom _] = return $ Bool True
testSymbol _        = return $ Bool False

testNumber :: [LispVal] -> ThrowsError LispVal
testNumber []         = throwError $ NumArgs 1 []
testNumber [Number _] = return $ Bool True
testNumber _          = return $ Bool False

testString :: [LispVal] -> ThrowsError LispVal
testString []         = throwError $ NumArgs 1 []
testString [String _] = return $ Bool True
testString _          = return $ Bool False

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom val] = return $ String val
symbolToString [notAtom]  = throwError $ TypeMismatch "symbol" notAtom
symbolToString args       = throwError $ NumArgs 1 args

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [String val] = return $ Atom val
stringToSymbol [notString]  = throwError $ TypeMismatch "string" notString
stringToSymbol args         = throwError $ NumArgs 1 args

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)]         = return x
car [DottedList (x:_) _] = return x
car [badArg]             = throwError $ TypeMismatch "pair" badArg
car badArgList           = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [x] y] = return y
cdr [DottedList (_:xs) y] = return $ DottedList xs y
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs]         = return $ List $ x:xs
cons [x, DottedList xs y] = return $ DottedList (x:xs) y
cons [x, y]               = return $ DottedList [x] y
cons badArgList           = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool x, Bool y]                   = return $ Bool $ x == y
eqv [Number x, Number y]               = return $ Bool $ x == y
eqv [String x, String y]               = return $ Bool $ x == y
eqv [Atom x, Atom y]                   = return $ Bool $ x == y
eqv [List x, List y]                   = return $ Bool $ (length x == length y) && all eqvPair (zip x y)
                                             where eqvPair (x1, x2) = case eqv [x1, x2] of
                                                                          Left err -> False
                                                                          Right (Bool val) -> val
eqv [DottedList xs x, DottedList ys y] = eqv [List $ x:xs, List $ y:ys]
eqv [_, _]                             = return $ Bool False
eqv badArgList                         = throwError $ NumArgs 2 badArgList

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) $ Map.lookup func primitives

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)

isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var) (liftIO . readIORef) $ lookup var env

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var) (liftIO . (`writeIORef` val)) $ lookup var env
    return val

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var val = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var val
        else liftIO $ do
            valueRef <- newIORef val
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef):env)
            return val

bindVar :: Env -> [(String, LispVal)] -> IO Env
bindVar envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where addBindings (var, val) = do
              ref <- newIORef val
              return (var, ref)
          extendEnv bindings env = fmap (++ env) (mapM addBindings bindings)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _)                         = return val
eval env val@(Number _)                         = return val
eval env val@(Bool _)                           = return val
eval env (Atom id)                              = getVar env id
eval env (List [Atom "quote", val])             = return val
eval env (List [Atom "if", pred, conseq, alt])  = do
                                                  result <- eval env pred
                                                  case result of
                                                      Bool False -> eval env alt
                                                      _          -> eval env conseq
eval env (List [Atom "set!", Atom var, form])   = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom func:args))                = mapM (eval env) args >>= liftThrows . apply func
eval env badForm                                = throwError $ BadSpecialForm "Unrecognized special form" badForm

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

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
    show (NumArgs expected found) = "Expected " ++ show expected ++ " args: found values " ++ unwordsList found
    show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
    show (Parser parseErr) = "Parse error at " ++ show parseErr
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func) = message ++ ": " ++ show func
    show (UnboundVar message varname) = message ++ ": " ++ varname

type ThrowsError = Either LispError

trapError action = catchError action $ return . show

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
