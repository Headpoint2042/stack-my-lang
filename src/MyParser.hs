module MyParser
    ( parseMyLang
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar)
import Text.Parsec.Combinator (many1)
import Control.Arrow (left)


-- int x = 1;
-- int x = true; aka int x = 1 ? NOT ALLOWED
-- int y = x;
-- int z = 1 + 1 * 2;
-- int z = (a = 1) - (b = 0) - 1; NOT ALLOWED
data Expr = Const Integer
          | Var String
          | Add Expr Expr
          | Mult Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Condition Condition

data Declaration = Primitive Scope Primitive
                 | Derived Scope Derived

data Primitive = Int  String Expr
               | Bool String Expr
               | Char String Char

-- Array name [sizes] ArrayList
-- String name value
data Derived = Array String [Integer] ArrayList
             | String String String

-- Array_List: type [values]
data ArrayList = Int [Expr]
               | Bool [Expr]
               | Char [Char]
               | String [String]

data Boolean = True
             | False

data Condition = Condition Operator Expr Expr
               | Bool Boolean

data Operator = Eq | Neq | Gt | Lt | Ge | Le | And | Or



num :: Parser Integer
num = do
    n <- many1 digit
    return (read n)

parseNumUntilEnd :: String -> Either ParseError Integer
parseNumUntilEnd = parse (num <* eof) "Todo: filename"

parseMyLang s = left show $ parseNumUntilEnd s

