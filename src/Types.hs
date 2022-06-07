module Types where

import Prelude hiding (log)
import Control.Exception

data Error = Error {errMsg :: String}
             deriving (Show)

instance Exception Error

data Binop
  = Plus
  | Minus
  | Mul
  deriving (Eq, Show)

type Id = String

-- | Expressions
data Expr
  = EInt Int
  | EVar Id
  | EBin Binop  Expr Expr
  | ELet Id     Expr  Expr
  | ELog String Expr
  deriving (Eq, Show)

-- | Values
data Value
  = VInt  Int
  | VThunk Env Expr
  | VErr String
  deriving (Eq, Show)

type Env = [(Id, Value)]

-- | Look up an identifier in the environment
lookupId :: Id -> Env -> Value
lookupId x ((y, v) : env)
  | x == y    = v
  | otherwise = lookupId x env
lookupId x [] = throw (Error ("unbound variable: " ++ x))

-- | Extend an environment with a new binding
extend :: Id -> Value -> Env -> Env
extend x v env = (x, v) : env