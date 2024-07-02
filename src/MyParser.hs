module MyParser where
    -- ( --MyLang
    --   compile
    --   , Program(..)
    --   , Block(..)
    --   , Statement(..)
    --   , Expr(..)
    --   , Condition(..)
    --   , Declaration(..)
    --   , Scope(..)
    --   , Primitive(..)
    --   , Derived(..)
    --   , DerivedType(..)
    --   , Assignment(..)
    -- ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar)
import Text.Parsec.Combinator (many1)
import Control.Arrow (left)

import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Maybe
import Data.List


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
          | StringLiteral String              -- Just a string "Some random text" 
        --   | Concat [Expr]              -- Operation of concatenation of lists
          | ArrayLiteral [Expr]               -- create an array with elements [3, 5, 90+13, 24, 15]
          | ArrayIndex String Expr -- get values of array: y = x[1]
          deriving (Show)


data Condition = Eq Condition Condition
               | Neq Condition Condition
               | Gt Condition Condition
               | Lt Condition Condition
               | Ge Condition Condition
               | Le Condition Condition
               | And Condition Condition
               | Or Condition Condition
               | Not Condition
               | Boolean Bool
               | Expr Expr
               deriving (Show)

-- data Order = Eq | Neq | Gt | Lt | Ge | Le


-- data Scope, between the {}
type Block = [Statement]

-- data Program = Program Block deriving (Show)
newtype Program = Program Block deriving (Show)

-- data Instruction, each line of code
data Statement = Declaration Declaration
               | Assignment  Assignment
               | If    Condition Block (Maybe Block)
               | While Condition Block
               | Print Expr
               | Fork
               | Join
              --  | Parbegin Integer
              --  | Parend
               | Lock String
               | Unlock String
               | Block Block
               deriving (Show)

-- Declaration of variables, helps backend --
data Declaration = Primitive Scope Primitive
               --   | Derived Scope Derived
                 | Derived Derived
                 deriving (Show)

data Scope = Global
           | Local
           deriving (Show)

-- If Maybe None then initialize primitive with predefined default value.
-- Add prefix T to denote Primitive types
data Primitive = PInt  String (Maybe Expr)    -- Can change it instead to be (Maybe Expr)
               | PBool String (Maybe Expr)
               | PChar String (Maybe Expr)
               | PLock String
               deriving (Show)

-- Array type name [sizes (must be integers!)] values
-- If Maybe None then initialize derived with predefined default value.
data Derived = Array DerivedType String Integer (Maybe Expr)
             | String String (Maybe Expr)
             deriving (Show)

-- add prefix T so that we don't have the same constructors as in Primitive
data DerivedType = DInt
                 | DBool
                 | DChar
                --  | DString
                deriving (Show)


-- Variable assignment
data Assignment = Absolute String Expr             -- Includes cases where x = y, x = y - 3 ...
                | Partial  String Expr Expr       -- Important for array value changing at index:  a[1] = 24
                deriving (Show)


languageDef = 
  emptyDef { Token.commentLine = "//"
           , Token.identStart = letter
           , Token.identLetter = alphaNum
           , Token.reservedNames = 
              [ "while", "if", "else", "int", "char", "bool", "String", "Lock", "lock", "unlock", "fork", "join", "global", "true", "false"]
           , Token.reservedOpNames = 
              [ "-", "+", "*", "/", "&&", "||", "==", "!=", "<", ">", "<=", ">=", "!"]
           , Token.caseSensitive = True
  }

lexer = Token.makeTokenParser languageDef

-- Create functions for all types of tokens
identifier    = Token.identifier lexer
integer       = Token.integer lexer
parens        = Token.parens lexer
braces        = Token.braces lexer
brackets      = Token.brackets lexer
symbol        = Token.symbol lexer
semi          = Token.semi lexer
dot           = Token.dot lexer
commaSep      = Token.commaSep lexer
charLiteral   = Token.charLiteral lexer
stringLiteral = Token.stringLiteral lexer
reserved      = Token.reserved lexer
whiteSpace    = Token.whiteSpace lexer


program :: Parser Program
program =  whiteSpace *> (Program <$> block) <* eof

-- we did not define block as = braces (many statement) because we need to use block for our global scope
block :: Parser Block
block = many statement

statement :: Parser Statement
statement =  try (Declaration <$> (declaration <* semi)) 
         <|> try (Assignment <$> (assignment <* semi))
         <|> try (If <$> (reserved "if" *> parens condition) <*> braces block <*> optionMaybe (reserved "else" *> braces block))
         <|> try (While <$> (reserved "while" *> parens condition) <*> braces block)
         <|> try (Print <$> (reserved "print" *> parens expr <* semi))
         <|> try (Fork <$ (reserved "fork" *> semi))
         <|> try (Join <$ (reserved "join" *> semi))
         <|> try (Lock <$> (identifier <* (dot *> reserved "lock" *> semi)))
         <|> try (Unlock <$> (identifier <* (dot *> reserved "unlock" *> semi)))
         <|> Block <$> braces block

declaration :: Parser Declaration
declaration =  try (Derived <$> derived)
           <|> Primitive <$> scope <*> primitive

scope :: Parser Scope
scope =  try (Global <$ reserved "global")
     <|> pure Local

primitive :: Parser Primitive
primitive =  try (PInt <$> (reserved "int" *> identifier) <*> optionMaybe (symbol "=" *> expr))
         <|> try (PBool <$> (reserved "bool" *> identifier) <*> optionMaybe (symbol "=" *> expr))
         <|> try (PChar <$> (reserved "char" *> identifier) <*> optionMaybe (symbol "=" *> expr))
         <|> PLock <$> (reserved "Lock" *> identifier)

derived :: Parser Derived
derived =  try (Array <$> derivedType <*> identifier <*> brackets integer <*> optionMaybe (symbol "=" *> expr))
       <|> String <$> (reserved "String" *> identifier) <*> optionMaybe (symbol "=" *> expr)

derivedType :: Parser DerivedType
derivedType =  try (DInt <$ reserved "int")
           <|> try (DBool <$ reserved "bool")
           <|> DChar <$ reserved "char"

-- name[index] = value
assignment :: Parser Assignment
assignment =  try (Partial <$> identifier <*> brackets expr <*> (symbol "=" *> expr))
          <|> Absolute <$> identifier <*> (symbol "=" *> expr)

fork :: Parser Statement
fork = Fork <$ reserved "fork"

join :: Parser Statement
join = Join <$ reserved "join"

expr :: Parser Expr
expr = chainl1 term (addOp <|> subOp)

-- term :: Parser Expr
-- term = try parseDiv 
--    <|> try mult
--    <|> factor
term :: Parser Expr
term = chainl1 factor (multOp <|> divOp)

factor :: Parser Expr
factor = try arrayLiteral
     <|> try arrayIndex
     <|> try (Condition <$> boolean)
     <|> try parseConst
     <|> try var
     <|> try parseChar
     <|> try parseString
     <|> try (parens expr)
     <|> parens (Condition <$> condition)

multOp :: Parser (Expr -> Expr -> Expr)
multOp = Mult <$ symbol "*"

divOp :: Parser (Expr -> Expr -> Expr)
divOp = Div <$ symbol "/"

parseConst :: Parser Expr
parseConst = Const <$> integer

var :: Parser Expr
var = Var <$> identifier

addOp :: Parser (Expr -> Expr -> Expr)
-- add = Add <$> (term <* symbol "+") <*> expr
addOp = Add <$ symbol "+"

subOp :: Parser (Expr -> Expr -> Expr)
subOp = Sub <$ symbol "-"

parseChar :: Parser Expr
parseChar = Char <$> charLiteral

-- stringLiteral -> from ParSec
parseString :: Parser Expr
parseString = StringLiteral <$> stringLiteral

arrayLiteral :: Parser Expr
arrayLiteral = ArrayLiteral <$> brackets (commaSep expr)

arrayIndex :: Parser Expr
arrayIndex = ArrayIndex <$> identifier <*> brackets expr

conditionExpr :: Parser Condition
conditionExpr =  try eq 
             <|> try neq
             <|> try gt 
             <|> try lt 
             <|> try ge 
             <|> try le 
             <|> try boolean
             <|> try (parens condition)
             <|> Expr <$> expr


condition :: Parser Condition
condition = try parseNot
        <|> chainl1 conditionExpr (parseAnd <|> parseOr)

-- var <|> parens condition

-- bool x  = (y == z == v)

eq :: Parser Condition
eq = Eq <$> ((Expr <$> expr) <* symbol "==") <*> conditionExpr

neq :: Parser Condition
neq = Neq <$> ((Expr <$> expr) <* symbol "!=") <*> conditionExpr

gt :: Parser Condition
gt = Gt <$> ((Expr <$> expr) <* symbol ">") <*> conditionExpr

lt :: Parser Condition
lt = Lt <$> ((Expr <$> expr) <* symbol "<") <*> conditionExpr

ge :: Parser Condition
ge = Ge <$> ((Expr <$> expr) <* symbol ">=") <*> conditionExpr

le :: Parser Condition
le = Le <$> ((Expr <$> expr) <* symbol ">=") <*> conditionExpr

parseAnd :: Parser (Condition -> Condition -> Condition)
parseAnd = And <$ symbol "&&"

parseOr :: Parser (Condition -> Condition -> Condition)
parseOr = Or <$ symbol "||" 

parseNot :: Parser Condition
parseNot = Not <$> (symbol "!" *> conditionExpr)

boolean :: Parser Condition
-- boolean = Boolean <$> (try ((reserved "true") *> pure True) <|> (reserved "false" *> pure False))
boolean =  try (Boolean <$> (reserved "true" *> pure True))
       <|> Boolean <$> (reserved "false" *> pure False)

compile :: FilePath -> IO (Either ParseError Program)
compile filePath = do
    input <- readFile filePath
    let res = parse program "" input
    return res

hashmapComp :: IO (Either ParseError Program) -> IO (Either ParseError Hashmap)
hashmapComp x = do
  result <- x
  return $ case result of
    Right program -> Right (createHashmap program)
    Left err -> Left err

-- compile "../test/TestLanguage.txt"

-- num :: Parser Integer
-- num = do
--     n <- many1 digit
--     return (read n)

-- NumUntilEnd :: String -> Either ParseError Integer
-- NumUntilEnd =  (num <* eof) "Todo: filename"

-- MyLang s = left show $ NumUntilEnd s



-- Type checking

-- variable hashmap

-- blocks(if while) condition checking if condition

-- global int x = 0;
-- { global int x = 2; }

-- 1. Check if variables are declared where needed.
-- 2. Check if referencing variables are according
-- 3. evaluateExpr
data Types = TArray DerivedType | TString | TInt | TBool | TChar | TLock deriving (Show)
data Hashmap = Scope [(Types, String)] [Hashmap] deriving (Show)

createHashmap :: Program -> Hashmap
createHashmap (Program x) = Scope (hashmapVar x) (hashmapBlock x) 


hashmapVar :: Block -> [(Types, String)]
hashmapVar [] = []
hashmapVar ((Declaration (Primitive _ x)):xs) = (getPrimitiveType x) : hashmapVar xs
hashmapVar ((Declaration (Derived x)):xs) = (getDerivedType x) : hashmapVar xs
hashmapVar (_:xs) = hashmapVar xs 

getPrimitiveType :: Primitive -> (Types, String)
getPrimitiveType (PInt x _ ) = (TInt, x)
getPrimitiveType (PBool x _) = (TBool, x)
getPrimitiveType (PChar x _) = (TChar, x)
getPrimitiveType (PLock x) = (TLock, x)

getDerivedType :: Derived -> (Types, String)
getDerivedType (Array (DInt) x _ _) = (TArray DInt, x)
getDerivedType (Array (DBool) x _ _) = (TArray DBool, x)
getDerivedType (Array (DChar) x _ _) = (TArray DChar, x)
getDerivedType (String x _) = (TString, x)


hashmapBlock :: Block -> [Hashmap]
hashmapBlock [] = []
hashmapBlock ((Block x):xs) = (Scope (hashmapVar x) (hashmapBlock x)) : hashmapBlock xs
hashmapBlock ((If _ x y):xs) 
  | isJust y  = (Scope (hashmapVar x) (hashmapBlock x)) : (Scope (hashmapVar (fromJust y)) (hashmapBlock (fromJust y))) : hashmapBlock xs
  | otherwise = (Scope (hashmapVar x) (hashmapBlock x)) : hashmapBlock xs
hashmapBlock ((While _ x):xs) = (Scope (hashmapVar x) (hashmapBlock x)) : hashmapBlock xs

-- checkRepeatVar :: [Hashmap] -> Bool
-- checkRepeatVar [] = True
-- checkRepeatVar ((Scope list scopes):xs)
--   | (hasUniqueElements (map snd list)) = && (checkRepeatVar scopes) && (checkRepeatVar xs)
--   where hasUniqueElements lst = length lst == length (nub lst)
-- occurrences :: Ord a => [a] -> [(Int, [a])]
-- occurrences like count or count_ but shows the list of elements that occur X times
-- occurrences "This is the test line" == [(1,"Tln"),(2,"h"),(3,"eist"),(4," ")]
