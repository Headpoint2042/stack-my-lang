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


------------------------------------------------
--                    EDSL                    --
------------------------------------------------

-- data Program = Program Block deriving (Show)
newtype Program = Program Block deriving (Show)

-- data Scope, between the {}
type Block = [Statement]
type VarName = String
type ArrSize = Integer

-- data Instruction, each line of code
data Statement = Declaration Declaration
               | Assignment  Assignment
               | If          Condition Block (Maybe Block)
               | While       Condition Block
               | Print       Expr
               | Thread      Block
               | Lock        VarName
               | Unlock      VarName
               | Block       Block
               deriving (Show)


data Scope  = Global | Local deriving (Show)
data MyType = TInt | TBool | TChar deriving (Show)

-- declaration of variables
data Declaration = Primitive Scope MyType VarName (Maybe Expr)
                 | TLock     VarName            -- always global
                 | Array     MyType VarName ArrSize (Maybe Expr)
                 | String    VarName Expr       -- Expr must be StringLiteral; String is immutable

-- assignment of variables
data Assignment = Absolute VarName Expr            -- includes cases where x = y, x = y - 3 ...
                | Partial  VarName Expr Expr       -- important for array value changing at index:  a[1] = 24
                deriving (Show)

-- Expr is rhs of =
data Expr = Const Integer
          | Char Char 
          | Var String
          | Add Expr Expr
          | Mult Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Condition Condition
          -- bonus
          | ArrayLiteral [Expr]              -- create an array with elements [3, 5, 90+13, 24, 15]
          | ArrayIndex String Expr           -- get values of array: y = x[1]
          | StringLiteral String             -- a string "Some random text" 
          deriving (Show)


-- 1 == 2 == False works like (1 == 2) == False -> False == False -> True
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


{-
   compile class: Statement, Declaration, Fork, 
-}


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

-- "bool x  = (y == z == v)"

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

-- hashmapComp :: IO (Either ParseError Program) -> IO (Either ParseError Hashmap)
-- hashmapComp x = do
--   result <- x
--   return $ case result of
--     Right program -> Right (createHashmap program)
--     Left err -> Left err

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
data Types = TArray DerivedType | TString | TInt | TBool | TChar | TLock deriving (Eq, Show)
data Hashmap = Scope [(Types, String)] [Hashmap] deriving (Show)

createHashmap :: Program -> Program
createHashmap (Program x) = Program (typeCheckingBlock x (Scope [] []) [])



-- 1. check redeclaration
checkReDeclaration :: [Statement] -> [String] -> Bool
checkReDeclaration [] _ = True
checkReDeclaration ((Declaration (Primitive s p)):xs) list 
   | elem name list = error ("Variable " ++ name ++ " was declared twice")
   | otherwise = checkReDeclaration xs (list ++ [name])
   where 
      (varType, name) = getPrimitiveType p 

checkReDeclaration ((Declaration (Derived p)):xs) list 
   | elem name list = error ("Variable " ++ name ++ " was declared twice")
   | otherwise = checkReDeclaration xs (list ++ [name])
   where 
      (varType, name) = getDerivedType p

checkReDeclaration ((Block block):xs) list = (checkReDeclaration block []) && (checkReDeclaration xs list)
checkReDeclaration ((If cond block mayBlock):xs) list
   | isJust mayBlock = x && (checkReDeclaration (fromJust mayBlock) [])
   | otherwise = x
   where 
      x = (checkReDeclaration block []) && (checkReDeclaration xs list)
checkReDeclaration ((While cond block):xs) list = (checkReDeclaration block []) && (checkReDeclaration xs list)
checkReDeclaration (_:xs) list = (checkReDeclaration xs list)

 
-- 2. renaming



-- function to do the type checking
typeCheckingBlock :: [Statement] -> Hashmap -> [String] -> [Statement]
typeCheckingBlock ((Declaration (Primitive scope x)):xs) h s 
   -- | checkReDeclaration h name = error ("Variable " ++ name ++ " was declared twice")
   | checkShadowing h name = typeCheckingBlock (renameVar name newName ((Declaration (Primitive scope x)):xs)) h (s ++ [name])
   | otherwise = []
   -- has to be continued with which other tests should be done
   where 
      (varType, name) = (getPrimitiveType x)
      newName = name ++ (show ((countElem name s) + 1))
typeCheckingBlock ((Declaration (Derived x)):xs) h s 
   -- | checkReDeclaration h name = error ("Variable " ++ name ++ " was declared twice")
   | checkShadowing h name = typeCheckingBlock (renameVar name newName ((Declaration (Derived x)):xs)) h (s ++ [name])
   | otherwise = []
   -- has to be continued with which other tests should be done
   where 
      (varType, name) = (getDerivedType x)
      newName = name ++ (show ((countElem name s) + 1))


-- Counts the number of occurences a element has in a list
countElem :: (Eq a) => a -> [a] -> Int
countElem x = length . filter (== x)

-- Checks whever the current scope does or does not have the variable name
-- checkReDeclaration :: Hashmap -> String -> Bool
-- checkReDeclaration (Scope list []) name = elem name (map snd list)
-- checkReDeclaration (Scope _ [s]) name = checkReDeclaration s name

-- Checks whever a variable is shadowing another one
checkShadowing :: Hashmap -> String -> Bool
checkShadowing (Scope list []) s = False
checkShadowing (Scope list [h]) s = elem s (map snd list) || checkShadowing h s

-- Inserts in the current scope the variable and its type
insertHashmap :: (Types, String) -> Hashmap -> Hashmap
insertHashmap v (Scope list []) = Scope (list ++ [v]) []
insertHashmap v (Scope list [s]) = Scope list [insertHashmap v s]

-- Goes through each type of statment and renames where possible with the new name
renameVar :: String -> String -> [Statement] -> [Statement]
renameVar name newName [] = []
renameVar name newName ((Declaration (Primitive scope (PInt x mayExpr))):xs) = (Declaration (Primitive scope (PInt (mattchingString name newName x) (renameMaybeExpr name newName mayExpr)))) : renameVar name newName xs
renameVar name newName ((Declaration (Primitive scope (PBool x mayExpr))):xs) = (Declaration (Primitive scope (PBool (mattchingString name newName x) (renameMaybeExpr name newName mayExpr)))) : renameVar name newName xs
renameVar name newName ((Declaration (Primitive scope (PChar x mayExpr))):xs) = (Declaration (Primitive scope (PChar (mattchingString name newName x) (renameMaybeExpr name newName mayExpr)))) : renameVar name newName xs
renameVar name newName ((Declaration (Primitive scope (PLock x))):xs) = (Declaration (Primitive scope (PLock (mattchingString name newName x)))) : renameVar name newName xs

renameVar name newName ((Declaration (Derived (Array n x i mayExpr))):xs) = (Declaration (Derived (Array n (mattchingString name newName x) i (renameMaybeExpr name newName mayExpr)))) : renameVar name newName xs
renameVar name newName ((Declaration (Derived (String x mayExpr))):xs) = (Declaration (Derived (String (mattchingString name newName x) (renameMaybeExpr name newName mayExpr)))) : renameVar name newName xs 

renameVar name newName ((Assignment (Absolute x expr)):xs) = Assignment (Absolute (mattchingString name newName x) (renameExpr name newName expr)) : renameVar name newName xs 
renameVar name newName ((Assignment (Partial x expr1 expr2)):xs) = Assignment (Partial (mattchingString name newName x ) (renameExpr name newName expr1) (renameExpr name newName expr2)) : renameVar name newName xs 

renameVar name newName ((If cond block mayBlock):xs)
   | isJust mayBlock = If (renameCond name newName cond) (renameVar name newName block) (Just (renameVar name newName (fromJust mayBlock))) : renameVar name newName xs 
   | otherwise = If (renameCond name newName cond) (renameVar name newName block) Nothing : renameVar name newName xs 

renameVar name newName ((While cond block):xs) = While (renameCond name newName cond) (renameVar name newName block) : renameVar name newName xs 
renameVar name newName ((Print x):xs) = (Print (renameExpr name newName x)) : renameVar name newName xs 

renameVar name newName ((Lock x):xs) = Lock (mattchingString name newName x ) : renameVar name newName xs 
renameVar name newName ((Unlock x):xs) = Unlock (mattchingString name newName x ) : renameVar name newName xs  
renameVar name newName ((Block block):xs) = Block (renameVar name newName block) : renameVar name newName xs
renameVar name newName (x:xs) = x : renameVar name newName xs 


-- Checks whether it is Nothing, then returns Nothing, otherwise tries to rename what is inside of expr
renameMaybeExpr :: String -> String -> Maybe Expr -> Maybe Expr
renameMaybeExpr name newName (Nothing) = Nothing
renameMaybeExpr name newName (Just expr) = Just (renameExpr name newName expr) 

-- Function compares name with a given string, and in case true then return the new name, otherwise return the default name
mattchingString :: String -> String -> String -> String
mattchingString name newName x 
   | x == name = newName
   | otherwise = name

-- Goes through each type of expression and renames where it is possible with the new name
renameExpr :: String -> String -> Expr -> Expr
renameExpr name newName (Var x) = Var (mattchingString name newName x)
renameExpr name newName (Add expr1 expr2) = Add (renameExpr name newName expr1) (renameExpr name newName expr2)
renameExpr name newName (Mult expr1 expr2) = Mult (renameExpr name newName expr1) (renameExpr name newName expr2)
renameExpr name newName (Sub expr1 expr2) = Sub (renameExpr name newName expr1) (renameExpr name newName expr2)
renameExpr name newName (Div expr1 expr2) = Div (renameExpr name newName expr1) (renameExpr name newName expr2)
renameExpr name newName (Condition cond) = Condition (renameCond name newName cond)
renameExpr name newName (ArrayLiteral list) = ArrayLiteral (renameExprList name newName list)
renameExpr name newName (ArrayIndex x expr) = ArrayIndex (mattchingString name newName x) (renameExpr name newName expr)
renameExpr name newName x = x

-- Goes through each expression in array and then applies renameExpr
renameExprList :: String -> String -> [Expr] -> [Expr]
renameExprList name newName [] = []
renameExprList name newName (expr:xs) = (renameExpr name newName expr) : renameExprList name newName xs

-- Goes through each type of expression and renames where it is possible with the new name
renameCond :: String -> String -> Condition -> Condition
renameCond name newName (Eq cond1 cond2) = Eq (renameCond name newName cond1) (renameCond name newName cond2)
renameCond name newName (Neq cond1 cond2) = Neq (renameCond name newName cond1) (renameCond name newName cond2)
renameCond name newName (Gt cond1 cond2) = Gt (renameCond name newName cond1) (renameCond name newName cond2)
renameCond name newName (Lt cond1 cond2) = Lt (renameCond name newName cond1) (renameCond name newName cond2)
renameCond name newName (Ge cond1 cond2) = Ge (renameCond name newName cond1) (renameCond name newName cond2)
renameCond name newName (Le cond1 cond2) = Le (renameCond name newName cond1) (renameCond name newName cond2)
renameCond name newName (And cond1 cond2) = And (renameCond name newName cond1) (renameCond name newName cond2)
renameCond name newName (Or cond1 cond2) = Or (renameCond name newName cond1) (renameCond name newName cond2)

renameCond name newName (Not cond) = Not (renameCond name newName cond)
renameCond name newName (Expr expr) = Expr (renameExpr name newName expr)
renameCond name newName x = x

-- Gets a primitive type and then exctracts from it the variable type and its name
getPrimitiveType :: Primitive -> (Types, String)
getPrimitiveType (PInt x _ ) = (TInt, x)
getPrimitiveType (PBool x _) = (TBool, x)
getPrimitiveType (PChar x _) = (TChar, x)
getPrimitiveType (PLock x) = (TLock, x)

-- Gets a derived type and then exctracts from it the variable type and its name
getDerivedType :: Derived -> (Types, String)
getDerivedType (Array (DInt) x _ _) = (TArray DInt, x)
getDerivedType (Array (DBool) x _ _) = (TArray DBool, x)
getDerivedType (Array (DChar) x _ _) = (TArray DChar, x)
getDerivedType (String x _) = (TString, x)

typeCheckingExpr :: Expr -> [Types] -> Hashmap -> Types
typeCheckingExpr (Var x) list scope 
   | elem typeVar list = typeVar
   | otherwise = error ("Was not expected " ++ getCorrectType typeVar)
   where
      typeVar = extractTypeHashmap scope x
typeCheckingExpr (Const _) list scope
   | elem TInt list = TInt
   | otherwise = error ("Was not expected " ++ getCorrectType TInt)
typeCheckingExpr (Add expr1 expr2) list scope
   | elem TInt list && typeCheckingExpr expr1 [TInt] scope == typeCheckingExpr expr2 [TInt] scope = TInt
   | otherwise = error ("Addition was not possible to different data types")
typeCheckingExpr (Sub expr1 expr2) list scope
   | elem TInt list && typeCheckingExpr expr1 [TInt] scope == typeCheckingExpr expr2 [TInt] scope = TInt
   | otherwise = error ("Substraction was not possible to different data types")
typeCheckingExpr (Mult expr1 expr2) list scope
   | elem TInt list && typeCheckingExpr expr1 [TInt] scope == typeCheckingExpr expr2 [TInt] scope = TInt
   | otherwise = error ("Multiplication was not possible to different data types")
typeCheckingExpr (Div expr1 expr2) list scope
   | elem TInt list && typeCheckingExpr expr1 [TInt] scope == typeCheckingExpr expr2 [TInt] scope = TInt
   | otherwise = error ("Division was not possible to different data types")
typeCheckingExpr (Condition cond) list scope
   | elem TBool list && typeCheckingCond cond scope = TBool
   | otherwise = error ("Was not expected " ++ getCorrectType TBool)
typeCheckingExpr (Char _) list scope
   | elem TChar list = TChar
   | otherwise = error ("Was not expected " ++ getCorrectType TChar)
typeCheckingExpr (StringLiteral _) list scope
   | elem TString list = TString
   | otherwise = error ("Was not expected " ++ getCorrectType TString)

typeCheckingCond :: Condition -> Hashmap -> Bool
typeCheckingCond (Boolean bool) scope = True

-- Dont allow empty arrays, ex:  int[0] = []
-- typeCheckingExpr (ArrayLiteral []) _ _= error ("Cannot declare empty arrays")
-- typeCheckingExpr (ArrayLiteral [x]) list scope
--    | 
--    where 
--       typeVar = typeCheckingExpr x list scope
-- typeCheckingExpr (ArrayLiteral (x:xs))


extractTypeHashmap :: Hashmap -> String -> Types
extractTypeHashmap (Scope list []) s
   | elem s (map snd list) = fst $ fromJust (find (\x -> (snd x == s)) list)
   | otherwise = error ("The variable has not been declared yet: " ++ s)
extractTypeHashmap (Scope list [h]) s
   | elem s (map snd list) = fst $ fromJust (find (\x -> (snd x == s)) list)
   | otherwise = extractTypeHashmap h s

getCorrectType :: Types -> String
getCorrectType TInt = "Int"
getCorrectType TBool = "Bool"
getCorrectType TChar = "Char"
getCorrectType TLock = "Lock"
getCorrectType TString = "String"
getCorrectType (TArray DInt) = "Int[]"
getCorrectType (TArray DBool) = "Bool[]"
getCorrectType (TArray DChar) = "Char[]"

