{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Parser (
    parseExpr
  , parseTokens
  ) where

import Lexer
import Types hiding (Nano (..))
import Control.Monad.Except
import Control.Exception

}

-- Entry point
%name top

-- Lexer structure
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    let   { LET _    }
    in    { IN _     }
    log   { LOG _    }
    TNUM  { NUM _ $$ }
    TSTR  { STRING _ $$ }
    ID    { ID _ $$  }    
    '='   { EQB _    }
    '+'   { PLUS _   }
    '-'   { MINUS _  }
    '*'   { MUL _    }
    '('   { LPAREN _ }
    ')'   { RPAREN _ }


-- Operators
%right in
%nonassoc '='
%left '+' '-'
%left '*' '/'
%%

Top  : ID '=' Expr                 { $3 }
     | Expr                        { $1 }

Expr : Expr '+'  Expr               { EBin Plus  $1 $3 }
     | Expr '-'  Expr               { EBin Minus $1 $3 }
     | Expr '*'  Expr               { EBin Mul   $1 $3 }
     | let ID '='  Expr in Expr     { ELet $2 $4 $6    }
     | log TSTR Expr                { ELog $2 $3     }
     | TNUM                         { EInt $1        }
     | '(' Expr ')'                 { $2             }
     | ID                           { EVar $1        }

{
parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError []     = throwError "Unexpected end of Input"

parseExpr :: String -> Expr
parseExpr s = case parseExpr' s of
                Left msg -> throw (Error ("parse error:" ++ msg))
                Right e  -> e

parseExpr' input = runExcept $ do
   tokenStream <- scanTokens input
   top tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens


}
