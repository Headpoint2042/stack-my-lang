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
data Expr = Const Integer
          | Var String
          | Add Expr Expr
          | Mult Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Condition Condition
          | Char Char                  
          | Concat [Expr]              -- Operation of concatenation of lists
          | String String              -- Just a string "Some random text" 
          | Array [Expr]               -- create an array with elements [3, 5, 90+13, 24, 15]
          | Array_Value String [Integer] -- get values of array: y = x[1]


data Boolean = True
             | False

data Condition = Operation Operator Expr Expr
               | And Condition Condition
               | Or Condition Condition
               | Bool Boolean
               | Not Condition

data Operator = Eq | Neq | Gt | Lt | Ge | Le


-- data Scope, between the {}
data Block = Instructions [Instruction]

-- data Instruction, each line of code
data Instruction = Declaration Declaration
                 | Assignment  Var_Assignment
                 | If_block    Condition Block
                 | While_block Condition Block
                 | Print       
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

data Primitive = Int  String Expr
               | Bool String Expr
               | Char String Expr
               | Lock String

-- Array type name [sizes] values
data Derived = Array Var_Type String [Integer] Expr
             | String String Expr

data Var_Type = Int
              | Bool
              | Char
              | String


-- Variable assignment
data Var_Assignment = Primitive String Expr             -- Includes cases where x = y, x = y - 3 ...
                    | Array String [Integer] Expr       -- Important for array value changing at index:  a[1] = 24


num :: Parser Integer
num = do
    n <- many1 digit
    return (read n)

parseNumUntilEnd :: String -> Either ParseError Integer
parseNumUntilEnd = parse (num <* eof) "Todo: filename"

parseMyLang s = left show $ parseNumUntilEnd s

