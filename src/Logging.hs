-------------------------------------------------------------------------------
-- DO NOT MODIFY THIS SEGMENT
-------------------------------------------------------------------------------

module Logging where

import Prelude hiding (log)  
import Control.Exception (throw, catch)
import Types
import Parser

-- | A logging computation contains a log (a list of messages) and a value
data Logging a = Logging [String] a
  deriving (Eq, Show)

-- | A computation that just logs `msg`
log :: String -> Logging ()
log msg = Logging [msg] () 

-- | Pretty-print a logging computation: 
-- | print the log messages and the computation result on separate lines
pretty :: Logging Value -> String
pretty (Logging log v) = unlines (log ++ [prettyVal v])
  where
    prettyVal (VInt x)     = show x
    prettyVal (VThunk _ _) = "<thunk>"
    prettyVal (VErr _)     = "<error>"

-- | Apply a binary operator to two values
evalOp :: Binop -> Value -> Value -> Value
evalOp Plus   (VInt n)     (VInt m)     = VInt  (n + m)
evalOp Minus  (VInt n)     (VInt m)     = VInt  (n - m)
evalOp Mul    (VInt n)     (VInt m)     = VInt  (n * m)
evalOp _      _        _                = throw (Error "type error: binop")

-- | For testing purposes
e1 = EBin Plus (EInt 2)                    -- No logging
               (EInt 3)
e2 = EBin Plus (ELog "I saw 2" (EInt 2))   -- Only log 2 
               (EInt 3)
e3 = EBin Plus (ELog "I saw 2" (EInt 2))   -- Log both 2 and 3 
               (ELog "I saw 3" (EInt 3))

-------------------------------------------------------------------------------
-- Task 3.1: Monad Instance for Logging
-------------------------------------------------------------------------------

instance Monad Logging where
  return x         = error "TBD..."

  step >>= process = error "TBD..."

-------------------------------------------------------------------------------
-- Task 3.2: Eval with logging
-------------------------------------------------------------------------------

eval :: Env -> Expr -> Logging Value
eval env e = error "TBD..."

-------------------------------------------------------------------------------
-- Task 3.3: Lazy evaluation
-------------------------------------------------------------------------------

-- | Evaluate an expression in an environment lazily,
-- | i.e. only evaluate let-definitions when they are needed in the final result
evalLazy :: Env -> Expr -> Logging Value
evalLazy env e = error "TBD..."

-------------------------------------------------------------------------------
-- DO NOT MODIFY THIS SEGMENT
-------------------------------------------------------------------------------

instance Functor Logging where
  fmap f (Logging log x) = Logging log (f x)

instance Applicative Logging where
  pure v = Logging [] v

  (Logging log1 f) <*> (Logging log2 x) = Logging (log1 ++ log2) (f x)


testFile :: FilePath -> IO String
testFile f = (readFile f >>= testString) `catch` exitError

execFile :: FilePath -> IO ()
execFile f = testFile f >>= putStr

execString :: String -> IO ()
execString s = testString s >>= putStr

testString :: String -> IO String
testString s = execExpr (parseExpr s) `catch` exitError

execExpr :: Expr -> IO String
execExpr e = (return $ pretty $ eval [] e) `catch` exitError

testFileLazy :: FilePath -> IO String
testFileLazy f = (readFile f >>= testStringLazy) `catch` exitError

execFileLazy :: FilePath -> IO ()
execFileLazy f = testFileLazy f >>= putStr

testStringLazy :: String -> IO String
testStringLazy s = execExprLazy (parseExpr s) `catch` exitError

execStringLazy :: String -> IO ()
execStringLazy s = testStringLazy s >>= putStr

execExprLazy :: Expr -> IO String
execExprLazy e = (return $ pretty $ evalLazy [] e) `catch` exitError

parse :: String -> Expr
parse = parseExpr

parseFile :: FilePath -> IO ()
parseFile f = readFile f >>= (print . parse)

exitError :: Error -> IO String
exitError (Error msg) = return msg
