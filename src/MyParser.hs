-- module MyParser
--     ( MyLang
--     ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar)
import Text.Parsec.Combinator (many1)
import Control.Arrow (left)

import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


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


data Condition = Eq Expr Expr
               | Neq Expr Expr
               | Gt Expr Expr
               | Lt Expr Expr
               | Ge Expr Expr
               | Le Expr Expr
               | And Condition Condition
               | Or Condition Condition
               | Not Condition
               | Boolean Bool
               deriving (Show)

-- data Order = Eq | Neq | Gt | Lt | Ge | Le


-- data Scope, between the {}
type Block = [Statement]

data Program = Program Block deriving (Show)

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
                deriving (Show)

-- Declaration of variables, helps backend --
data Declaration = Primitive Scope Primitive
                 | Derived Scope Derived
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


program :: Parser Program
program = Program <$> block <* eof

-- we did not define block as = braces (many statement) because we need to use block for our global scope
block :: Parser Block
block = many statement

statement :: Parser Statement
statement =  try (Declaration <$> (declaration <* semi)) 
         <|> try (Assignment <$> (assignment <* semi))
         <|> try (If <$> (reserved "if" *> parens condition) <*> braces block <*> (optionMaybe (reserved "else" *> braces block)))
         <|> try (While <$> (reserved "while" *> parens condition) <*> braces block)
         <|> try (Print <$> (reserved "print" *> parens expr <* semi))
         <|> try (Fork <$ (reserved "fork" *> semi))
         <|> try (Join <$ (reserved "join" *> semi))
         <|> try (Lock <$> (identifier <* (dot *> reserved "lock" *> semi)))
         <|> try (Unlock <$> (identifier <* (dot *> reserved "unlock" *> semi)))
         <|> Block <$> braces block

declaration :: Parser Declaration
declaration =  try (Derived <$> scope <*> derived)
           <|> Primitive <$> scope <*> primitive

scope :: Parser Scope
scope =  try (Global <$ (reserved "global"))
     <|> pure Local

primitive :: Parser Primitive
primitive =  try (PInt <$> (reserved "int" *> identifier) <*> (optionMaybe (symbol "=" *> expr)))
         <|> try (PBool <$> (reserved "bool" *> identifier) <*> (optionMaybe (symbol "=" *> expr)))
         <|> try (PChar <$> (reserved "char" *> identifier) <*> (optionMaybe (symbol "=" *> expr)))
         <|> PLock <$> (reserved "Lock" *> identifier)

derived :: Parser Derived
derived =  try (Array <$> derivedType <*> identifier <*> (brackets integer) <*> (optionMaybe (symbol "=" *> expr)))
       <|> String <$> (reserved "String" *> identifier) <*> (optionMaybe (symbol "=" *> expr))

derivedType :: Parser DerivedType
derivedType =  try (DInt <$ reserved "int")
           <|> try (DBool <$ reserved "bool")
           <|> DChar <$ reserved "char"

-- name[index] = value
assignment :: Parser Assignment
assignment =  try (Partial <$> identifier <*> brackets expr <*> (symbol "=" *> expr))
          <|> Absolute <$> identifier <*> (symbol "=" *> expr)

fork :: Parser Statement
fork = Fork <$ (reserved "fork")

join :: Parser Statement
join = Join <$ (reserved "join")

expr :: Parser Expr
expr = try add
   <|> try sub
   <|> term
   
term :: Parser Expr
term = try mult
   <|> try parseDiv
   <|> factor

factor :: Parser Expr
factor = arrayLiteral
     <|> try arrayIndex
     <|> try boolean
     <|> try (parens exprCond)
     <|> try var
     <|> try parseChar
     <|> try parseString
     <|> try (parens expr)
     <|> parseConst


parseConst :: Parser Expr
parseConst = Const <$> integer

var :: Parser Expr
var = Var <$> identifier

add :: Parser Expr
add = Add <$> (term <* symbol "+") <*> expr

mult :: Parser Expr
mult = Mult <$> (factor <* symbol "*") <*> term

sub :: Parser Expr
sub = Sub <$> (term <* symbol "-") <*> expr

parseDiv :: Parser Expr
parseDiv = Div <$> (factor <* symbol "/") <*> term

exprCond :: Parser Expr
exprCond = Condition <$> condition

parseChar :: Parser Expr
parseChar = Char <$> charLiteral

parseString :: Parser Expr
parseString = StringLiteral <$> stringLiteral

arrayLiteral :: Parser Expr
arrayLiteral = ArrayLiteral <$> (brackets (commaSep expr))

arrayIndex :: Parser Expr
arrayIndex = ArrayIndex <$> identifier <*> brackets expr

condition :: Parser Condition
condition =  try eq 
         <|> try neq 
         <|> try gt 
         <|> try lt 
         <|> try ge 
         <|> try le 
         <|> try parseAnd 
         <|> try parseOr 
         <|> try parseNot 
         <|> boolean

eq :: Parser Condition
eq = Eq <$> (expr <* symbol "==") <*> expr

neq :: Parser Condition
neq = Neq <$> (expr <* symbol "!=") <*> expr

gt :: Parser Condition
gt = Gt <$> (expr <* symbol ">") <*> expr

lt :: Parser Condition
lt = Lt <$> (expr <* symbol "<") <*> expr

ge :: Parser Condition
ge = Ge <$> (expr <* symbol ">=") <*> expr

le :: Parser Condition
le = Le <$> (expr <* symbol ">=") <*> expr

parseAnd :: Parser Condition
parseAnd = And <$> condition <*> ((symbol "&&") *> condition)

parseOr :: Parser Condition
parseOr = Or <$> condition <*> ((symbol "||") *> condition)

parseNot :: Parser Condition
parseNot = Not <$> ((symbol "!") *> condition)

boolean :: Parser Condition
boolean = Boolean <$> (try ((reserved "true") *> pure True) <|> (reserved "false" *> pure False))

compile :: FilePath -> IO (Either ParseError Program)
compile filePath = do
    input <- readFile filePath
    let res = parse program "" input
    return res

-- compile "../test/TestLanguage.txt"

-- num :: Parser Integer
-- num = do
--     n <- many1 digit
--     return (read n)

-- NumUntilEnd :: String -> Either ParseError Integer
-- NumUntilEnd =  (num <* eof) "Todo: filename"

-- MyLang s = left show $ NumUntilEnd s

