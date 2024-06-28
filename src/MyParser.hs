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
-- Still need to check if Array [Expr] all expr are the same type

-- Expr is rhs of =
data Expr = Const Integer
          | Var String
          | Add Expr Expr
          | Mult Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          
          | Condition Condition

          | Char Char                  
          | String String              -- Just a string "Some random text" 
        --   | Concat [Expr]              -- Operation of concatenation of lists
          | Array [Expr]               -- create an array with elements [3, 5, 90+13, 24, 15]
          | ArrayIndex String Expr -- get values of array: y = x[1]


data Condition = Comparison Operator Expr Expr
               | And Condition Condition
               | Or Condition Condition
               | Not Condition
               | Boolean Bool

data Operator = Eq | Neq | Gt | Lt | Ge | Le


-- data Scope, between the {}
type Block = [Statement]

data Program = Program Block

-- data Instruction, each line of code
data Statement = Declaration Declaration
                 | Assignment  Assignment
                 | If    Condition Block (Maybe Block)
                 | While Condition Block
                 | Print Expr
                 | Fork
                 | Join
                 | Lock String
                 | Unlock String
                 | Block Block

-- Declaration of variables, helps backend --
data Declaration = Primitive Scope Primitive
                 | Derived Scope Derived

data Scope = Global
           | Local

-- If Maybe None then initialize primitive with predefined default value.
data Primitive = Int  String (Maybe Expr)    -- Can change it instead to be (Maybe Expr)
               | Bool String (Maybe Expr)
               | Char String (Maybe Expr)
               | TLock String

-- Array type name [sizes (must be integers!)] values
-- If Maybe None then initialize derived with predefined default value.
data Derived = Array Type String [Integer] (Maybe Expr)
             | String String (Maybe Expr)

data Type = Int
          | Bool
          | Char
          | String


-- Variable assignment
data Assignment = Absolute String Expr             -- Includes cases where x = y, x = y - 3 ...
                | Partial  String [Expr] Expr       -- Important for array value changing at index:  a[1] = 24


num :: Parser Integer
num = do
    n <- many1 digit
    return (read n)

parseNumUntilEnd :: String -> Either ParseError Integer
parseNumUntilEnd = parse (num <* eof) "Todo: filename"

parseMyLang s = left show $ parseNumUntilEnd s

