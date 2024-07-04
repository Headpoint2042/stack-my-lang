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
                 deriving (Show)

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
          | ArrayIndex VarName Expr           -- get values of array: y = x[1]
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
              [ "while", "if", "else", "int", "char", "bool", "String", "Lock", "lock", "unlock", "thread", "global", "true", "false"]
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
         <|> try (Thread <$> (reserved "thread" *> braces block))
         <|> try (Lock <$> (identifier <* (dot *> reserved "lock" *> semi)))
         <|> try (Unlock <$> (identifier <* (dot *> reserved "unlock" *> semi)))
         <|> Block <$> braces block

declaration :: Parser Declaration
declaration = try (Array <$> parserType <*> identifier <*> brackets integer <*> optionMaybe (symbol "=" *> expr))
          <|> try (TLock <$> (reserved "Lock" *> identifier))
          <|> try (Primitive <$> scope <*> parserType <*> identifier <*> optionMaybe (symbol "=" *> expr))
          <|> String <$> (reserved "String" *> identifier) <*> (reserved "=" *> expr)

scope :: Parser Scope
scope =  try (Global <$ reserved "global")
     <|> pure Local

parserType :: Parser MyType
parserType = try (TInt <$ reserved "int")
         <|> try (TBool <$ reserved "bool")
         <|> TChar <$ reserved "char"

-- name[index] = value
assignment :: Parser Assignment
assignment =  try (Partial <$> identifier <*> brackets expr <*> (symbol "=" *> expr))
          <|> Absolute <$> identifier <*> (symbol "=" *> expr)

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
conditionExpr =  try (parens condition)
             <|> try (chainl1 (Expr <$> expr) (eq <|> neq <|> gt <|> lt <|> ge <|> le))
             <|> boolean


condition :: Parser Condition
condition = try parseNot
        <|> chainl1 conditionExpr (parseAnd <|> parseOr)

-- var <|> parens condition

-- "bool x  = (y == z == v)"

eq :: Parser (Condition -> Condition -> Condition)
eq = Eq <$ symbol "=="

neq :: Parser (Condition -> Condition -> Condition)
neq = Neq <$ symbol "!="

gt :: Parser (Condition -> Condition -> Condition)
gt = Gt <$ symbol ">"

lt :: Parser (Condition -> Condition -> Condition)
lt = Lt <$ symbol "<"

ge :: Parser (Condition -> Condition -> Condition)
ge = Ge <$ symbol ">="

le :: Parser (Condition -> Condition -> Condition)
le = Le <$ symbol "<="

parseAnd :: Parser (Condition -> Condition -> Condition)
parseAnd = And <$ symbol "&&"

parseOr :: Parser (Condition -> Condition -> Condition)
parseOr = Or <$ symbol "||" 

parseNot :: Parser Condition
parseNot = Not <$> (symbol "!" *> parens condition)

boolean :: Parser Condition
-- boolean = Boolean <$> (try ((reserved "true") *> pure True) <|> (reserved "false" *> pure False))
boolean =  try (Boolean <$> (reserved "true" *> pure True))
       <|> Boolean <$> (reserved "false" *> pure False)

compile :: FilePath -> IO (Either String Program)
compile filePath = do
    input <- readFile filePath
    let res = parse program "" input
    case res of
        Left err -> return (Left (show err))
        Right prog -> do
            typeCheckedResult <- try (evaluate (typeCheckingProgram prog)) :: IO (Either SomeException Program)
            case typeCheckedResult of
                Left ex -> return (Left ("Type error: " ++ show ex))
                Right checkedProg -> return (Right checkedProg)

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


data HashmapType = HInt | HBool | HChar | HLock | HString | HArray HashmapType  deriving (Show, Eq)
data Hashmap = Scope [(HashmapType, VarName)] [Hashmap] deriving (Show)


typeCheckingProgram :: Program -> Program
typeCheckingProgram (Program block)
   | checkReDeclaration block [] = Program (typeCheckingBlock block (Scope [] []) [])
   | otherwise = error ("The program has variables declared multiple times in the same scope")










changeType :: MyType -> HashmapType
changeType TInt = HInt
changeType TChar = HChar
changeType TBool = HBool




-- Check declaration arrays size is the same
checkArraySize :: Declaration -> Bool
checkArraySize (Array _ _ size (Just (ArrayLiteral array))) 
   | size == 0 = error ("Cannot give array size 0")
   | toInteger actualSize == size = True
   | otherwise = error ("Difference between the size of the array " ++ show size ++ ", and the number of the elements given " ++ show actualSize)
   where
      actualSize = length array
checkArraySize (Array _ _ size _) 
   | size == 0 = error ("Cannot give array size 0")
   | otherwise = True
checkArraySize _ = True


checkThread :: [Statement] -> Bool
checkThread [] = True
checkThread ((Declaration (TLock _)):xs) = error ("Cannot declare Lock inside a thread block")
checkThread ((Declaration (Primitive Global _ _ _)):xs) = error ("Cannot declare global variables inside a thread block")
checkThread ((If _ block mayBlock):xs)
   | isJust mayBlock = checkThread block && checkThread (fromJust mayBlock) && checkThread xs
   | otherwise = checkThread block && checkThread xs
checkThread ((While _ block):xs) = checkThread block && checkThread xs
checkThread ((Block block):xs) = checkThread block && checkThread xs
checkThread (_:xs) = checkThread xs

checkWhile :: [Statement] -> Bool
checkWhile [] = True
checkWhile ((Declaration (TLock _):xs)) = error ("Cannot declare Lock inside a while block")
checkWhile ((Declaration (Primitive Global _ _ _)):xs) = error ("Cannot declare global variables inside while block")
checkWhile ((Thread block):xs) = error ("Cannot declare threads inside while block")
checkWhile ((If _ block mayBlock):xs)
   | isJust mayBlock = checkWhile block && checkWhile (fromJust mayBlock) && checkWhile xs
   | otherwise = checkWhile block && checkWhile xs
checkWhile ((Block block):xs) = checkWhile block && checkWhile xs
checkWhile ((While _ block):xs) = checkWhile block && checkWhile xs
checkWhile (_:xs) = checkWhile xs

checkStringDecl :: Declaration -> Bool
checkStringDecl (String name (StringLiteral _)) = True
checkStringDecl (String _ _) = error ("Did not declare the string correctly")
checkStringDecl _ = True


-- 1. check redeclaration
checkReDeclaration :: [Statement] -> [String] -> Bool
checkReDeclaration [] _ = True
checkReDeclaration ((Declaration (Primitive _ _ name _)):xs) list 
   | elem name list = error ("Variable " ++ name ++ " was declared twice")
   | otherwise = checkReDeclaration xs (list ++ [name])
checkReDeclaration ((Declaration (TLock name)):xs) list 
   | elem name list = error ("Variable " ++ name ++ " was declared twice")
   | otherwise = checkReDeclaration xs (list ++ [name])
checkReDeclaration ((Declaration (Array _ name _ _)):xs) list 
   | elem name list = error ("Variable " ++ name ++ " was declared twice")
   | otherwise = checkReDeclaration xs (list ++ [name])
checkReDeclaration ((Declaration (String name _)):xs) list 
   | elem name list = error ("Variable " ++ name ++ " was declared twice")
   | otherwise = checkReDeclaration xs (list ++ [name])
checkReDeclaration ((Thread block):xs) list = (checkReDeclaration block []) && (checkReDeclaration xs list)
checkReDeclaration ((Block block):xs) list = (checkReDeclaration block []) && (checkReDeclaration xs list)
checkReDeclaration ((If cond block mayBlock):xs) list
   | isJust mayBlock = x && (checkReDeclaration (fromJust mayBlock) [])
   | otherwise = x
   where 
      x = (checkReDeclaration block []) && (checkReDeclaration xs list)
checkReDeclaration ((While cond block):xs) list = (checkReDeclaration block []) && (checkReDeclaration xs list)
checkReDeclaration (_:xs) list = (checkReDeclaration xs list)


typeCheckingExpr :: Expr -> [HashmapType] -> Hashmap -> HashmapType
typeCheckingExpr (Const _) list hashmap 
   | elem HInt list = HInt
   | otherwise = error (errorExpected HInt)
typeCheckingExpr (Char _) list hashmap
   | elem HChar list = HChar
   | otherwise = error (errorExpected HChar)
typeCheckingExpr (Var x) list hashmap
   | elem varType list = varType
   | otherwise = error (errorExpected varType)
   where
      varType = extractTypeHashmap hashmap x
typeCheckingExpr (Add expr1 expr2) list hashmap
   | typeCheckingExpr expr1 [HInt] hashmap == typeCheckingExpr expr2 [HInt] hashmap && elem HInt list = HInt
   | otherwise = error (errorExpected HInt)
   where 
typeCheckingExpr (Sub expr1 expr2) list hashmap
   | typeCheckingExpr expr1 [HInt] hashmap == typeCheckingExpr expr2 [HInt] hashmap && elem HInt list = HInt
   | otherwise = error (errorExpected HInt)
typeCheckingExpr (Div expr1 expr2) list hashmap
   | typeCheckingExpr expr1 [HInt] hashmap == typeCheckingExpr expr2 [HInt] hashmap && elem HInt list = HInt
   | otherwise = error (errorExpected HInt)
typeCheckingExpr (Mult expr1 expr2) list hashmap
   | typeCheckingExpr expr1 [HInt] hashmap == typeCheckingExpr expr2 [HInt] hashmap && elem HInt list = HInt
   | otherwise = error (errorExpected HInt)
typeCheckingExpr (Condition cond) list hashmap
   | typeCheckingCond cond [HBool] hashmap == HBool && elem HBool list = HBool
   | otherwise = error (errorExpected HBool)
typeCheckingExpr (ArrayLiteral exprList) list hashmap
   | length exprList == 0 = error ("The array cannot be empty")
   | otherwise = varType
   where
      varType = typeCheckingListExpr exprList list hashmap
typeCheckingExpr (ArrayIndex x expr) list hashmap
   | elem varType list && typeCheckingExpr expr [HInt] hashmap == HInt = varType
   | otherwise = error (errorExpected varType)
   where
      HArray varType = extractTypeHashmap hashmap x
typeCheckingExpr (StringLiteral _) list hashmap
   | elem HString list = HString
   | otherwise = error (errorExpected HString)

typeCheckingCond :: Condition -> [HashmapType] -> Hashmap -> HashmapType
typeCheckingCond (Eq cond1 cond2) list hashmap 
   | cond1Type /= cond2Type = error ("Comparing different data types")
   | elem HBool list = HBool
   | otherwise = error (errorExpected HBool)
   where 
      cond1Type = typeCheckingCond cond1 [HInt, HBool, HChar, HArray HInt, HArray HChar, HArray HBool, HString] hashmap
      cond2Type = typeCheckingCond cond2 [HInt, HBool, HChar, HArray HInt, HArray HChar, HArray HBool, HString] hashmap
typeCheckingCond (Neq cond1 cond2) list hashmap
   | cond1Type /= cond2Type = error ("Comparing different data types")
   | elem HBool list = HBool
   | otherwise = error (errorExpected HBool)
   where 
      cond1Type = typeCheckingCond cond1 [HInt, HBool, HChar, HArray HInt, HArray HChar, HArray HBool, HString] hashmap
      cond2Type = typeCheckingCond cond2 [HInt, HBool, HChar, HArray HInt, HArray HChar, HArray HBool, HString] hashmap
typeCheckingCond (Gt cond1 cond2) list hashmap
   | cond1Type /= cond2Type = error ("Comparing different data types")
   | elem HBool list = HBool
   | otherwise = error (errorExpected HBool)
   where 
      cond1Type = typeCheckingCond cond1 [HInt, HChar] hashmap
      cond2Type = typeCheckingCond cond2 [HInt, HChar] hashmap
typeCheckingCond (Lt cond1 cond2) list hashmap
   | cond1Type /= cond2Type = error ("Comparing different data types")
   | elem HBool list = HBool
   | otherwise = error (errorExpected HBool)
   where 
      cond1Type = typeCheckingCond cond1 [HInt, HChar] hashmap
      cond2Type = typeCheckingCond cond2 [HInt, HChar] hashmap
typeCheckingCond (Ge cond1 cond2) list hashmap
   | cond1Type /= cond2Type = error ("Comparing different data types")
   | elem HBool list = HBool
   | otherwise = error (errorExpected HBool)
   where 
      cond1Type = typeCheckingCond cond1 [HInt, HChar] hashmap
      cond2Type = typeCheckingCond cond2 [HInt, HChar] hashmap
typeCheckingCond (Le cond1 cond2) list hashmap
   | cond1Type /= cond2Type = error ("Comparing different data types")
   | elem HBool list = HBool
   | otherwise = error (errorExpected HBool)
   where 
      cond1Type = typeCheckingCond cond1 [HInt, HChar] hashmap
      cond2Type = typeCheckingCond cond2 [HInt, HChar] hashmap
typeCheckingCond (And cond1 cond2) list hashmap
   | cond1Type /= cond2Type = error ("Comparing different data types")
   | elem HBool list = HBool
   | otherwise = error (errorExpected HBool)
   where 
      cond1Type = typeCheckingCond cond1 [HBool] hashmap
      cond2Type = typeCheckingCond cond2 [HBool] hashmap
typeCheckingCond (Or cond1 cond2) list hashmap
   | cond1Type /= cond2Type = error ("Comparing different data types")
   | elem HBool list = HBool
   | otherwise = error (errorExpected HBool)
   where 
      cond1Type = typeCheckingCond cond1 [HBool] hashmap
      cond2Type = typeCheckingCond cond2 [HBool] hashmap
typeCheckingCond (Not cond) list hashmap
   | elem HBool list && HBool == typeCheckingCond cond [HBool] hashmap = HBool
   | otherwise = error (errorExpected HBool)
typeCheckingCond (Boolean _) list hashmap
   | elem HBool list = HBool
   | otherwise = error (errorExpected HBool)
typeCheckingCond (Expr expr) list hashmap = typeCheckingExpr expr list hashmap

typeCheckingMayExpr :: Maybe Expr -> [HashmapType] -> Hashmap -> Bool
typeCheckingMayExpr mayExpr list hashmap
   | isJust mayExpr && elem (typeCheckingExpr (fromJust mayExpr) list hashmap) list = True
   | otherwise = True



errorExpected :: HashmapType -> String
errorExpected varType = ("Was not expected " ++ getStringType varType) 

typeCheckingListExpr :: [Expr] -> [HashmapType] -> Hashmap -> HashmapType
typeCheckingListExpr [x] list hashmap = HArray (typeCheckingExpr x [y | (HArray y) <- list] hashmap)
typeCheckingListExpr (x:xs) list hashmap
   | HArray (resType) == typeCheckingListExpr xs list hashmap = HArray (resType)
   | otherwise = error ("Array has different data types")
   where
      newList = [y | (HArray y) <- list] 
      resType = typeCheckingExpr x newList hashmap
 
-- 2. renaming

-- function to do the type checking
typeCheckingBlock :: [Statement] -> Hashmap -> [VarName] -> [Statement]
typeCheckingBlock [] _ _ = []
typeCheckingBlock ((Declaration (Primitive scope varType name mayExpr)):xs) hashmap list 
   | checkShadowing hashmap name = typeCheckingBlock (renameVar name newName ((Declaration (Primitive scope varType name mayExpr)):xs)) hashmap (list ++ [newName])
   | typeCheckingMayExpr mayExpr [changeType varType] hashmap = (Declaration (Primitive scope varType name mayExpr)) : typeCheckingBlock xs (insertHashmap (changeType varType, name) hashmap) list
   | otherwise = error ("Unexpected error: " ++ show (Declaration (Primitive scope varType name mayExpr)))
   where 
      newName 
         | elem name list = nameStart ++ show ((read [num]) + 1)
         | otherwise = name ++ "1"
      (nameStart, num) = (init (name :: String), last (name :: String))
typeCheckingBlock ((Declaration (TLock name)):xs) hashmap list
   | checkShadowing hashmap name = typeCheckingBlock (renameVar name newName ((Declaration (TLock name)):xs)) hashmap (list ++ [newName])
   | otherwise = (Declaration (TLock name)) : typeCheckingBlock xs (insertHashmap (HLock, name) hashmap) list
   where
      newName 
         | elem name list = nameStart ++ show ((read [num]) + 1)
         | otherwise = name ++ "1"
      (nameStart, num) = (init (name :: String), last (name :: String))
typeCheckingBlock ((Declaration (Array varType name size mayExpr)):xs) hashmap list
   | checkShadowing hashmap name = typeCheckingBlock (renameVar name newName ((Declaration (Array varType name size mayExpr)):xs)) hashmap (list ++ [newName])
   | checkArraySize (Array varType name size mayExpr) && typeCheckingMayExpr mayExpr [HArray (changeType varType)] hashmap = (Declaration (Array varType name size mayExpr)) : typeCheckingBlock xs (insertHashmap (changeType varType, name) hashmap) list
   | otherwise = error ("Unexpected error: " ++ show (Declaration (Array varType name size mayExpr)))
   where
      newName 
         | elem name list = nameStart ++ show ((read [num]) + 1)
         | otherwise = name ++ "1"
      (nameStart, num) = (init (name :: String), last (name :: String))
typeCheckingBlock ((Declaration (String name expr)):xs) hashmap list
   | checkShadowing hashmap name = typeCheckingBlock (renameVar name newName ((Declaration (String name expr)):xs)) hashmap (list ++ [newName])
   | checkStringDecl (String name expr) = (Declaration (String name expr)) : typeCheckingBlock xs (insertHashmap (HString, name) hashmap) list
   | otherwise = error ("Unexpected error: " ++ show (Declaration (String name expr)))
   where 
      newName 
         | elem name list = nameStart ++ show ((read [num]) + 1)
         | otherwise = name ++ "1"
      (nameStart, num) = (init (name :: String), last (name :: String))


typeCheckingBlock ((Assignment (Absolute name expr)):xs) hashmap list
   | elem varType [HInt, HBool, HChar] && typeCheckingMayExpr (Just expr) [varType] hashmap = (Assignment (Absolute name expr)) : typeCheckingBlock xs hashmap list
   | otherwise = error ("Cannot do absolute assignments to " ++ getStringType varType)
   where
      varType = extractTypeHashmap hashmap name
typeCheckingBlock ((Assignment (Partial name index expr)):xs) hashmap list
   | typeCheckingMayExpr (Just index) [HInt] hashmap && elem varType [HString, HArray HInt, HArray HBool, HArray HChar] && typeCheckingMayExpr (Just expr) [varType] hashmap = (Assignment (Partial name index expr)) : typeCheckingBlock xs hashmap list
   | otherwise = error ("Cannot do partial assignments to " ++ getStringType varType)
   where
      varType = extractTypeHashmap hashmap name

typeCheckingBlock ((If cond block mayBlock):xs) hashmap list
   | isJust mayBlock && typeCheckingCond cond [HBool] hashmap == HBool = (If cond (typeCheckingBlock block (newBlockHashmap hashmap) list) (Just (typeCheckingBlock (fromJust mayBlock) (newBlockHashmap hashmap) list))) : typeCheckingBlock xs hashmap list
   | typeCheckingCond cond [HBool] hashmap == HBool = (If cond (typeCheckingBlock block (newBlockHashmap hashmap) list) Nothing) : typeCheckingBlock xs hashmap list
   | otherwise = error ("Was expected condition in if statement")
typeCheckingBlock ((While cond block):xs) hashmap list
   | typeCheckingCond cond [HBool] hashmap == HBool && checkWhile block = (While cond (typeCheckingBlock block (newBlockHashmap hashmap) list)) : typeCheckingBlock xs hashmap list
   | otherwise = error ("Was expected condition in while statement")
typeCheckingBlock ((Print expr):xs) hashmap list
   | typeCheckingMayExpr (Just expr) [HInt, HBool, HChar, HArray HInt, HArray HChar, HArray HBool, HString] hashmap = (Print expr) : typeCheckingBlock xs hashmap list
   | otherwise = error ("Printing expression has to have the same data type")
typeCheckingBlock ((Thread block):xs) hashmap list
   | checkThread block = (Thread (typeCheckingBlock block (newBlockHashmap hashmap) list)) : typeCheckingBlock xs hashmap list
   | otherwise = error ("Unexpected syntax inside threads")
typeCheckingBlock ((Lock name):xs) hashmap list
   | varType == HLock = (Lock name) : typeCheckingBlock xs hashmap list
   | otherwise = error ("Cannot lock data type variable " ++ getStringType varType)
   where 
      varType = extractTypeHashmap hashmap name
typeCheckingBlock ((Unlock name):xs) hashmap list
   | varType == HLock = (Lock name) : typeCheckingBlock xs hashmap list
   | otherwise = error ("Cannot unlock data type variable " ++ getStringType varType)
   where 
      varType = extractTypeHashmap hashmap name
typeCheckingBlock ((Block block):xs) hashmap list= Block (typeCheckingBlock block (newBlockHashmap hashmap) list) : typeCheckingBlock xs hashmap list




-- Creates a new empty scope at the top of the hashmap
newBlockHashmap :: Hashmap -> Hashmap
newBlockHashmap (Scope list []) = (Scope list [Scope [] []])
newBlockHashmap (Scope list [s]) = (Scope list [newBlockHashmap s])



-- -- Inserts in the current scope the variable and its type
insertHashmap :: (HashmapType, String) -> Hashmap -> Hashmap
insertHashmap v (Scope list []) = Scope (list ++ [v]) []
insertHashmap v (Scope list [s]) = Scope list [insertHashmap v s]

-- Checks whever a variable is shadowing another one
checkShadowing :: Hashmap -> VarName -> Bool
checkShadowing (Scope list []) s = False
checkShadowing (Scope list [h]) s = elem s (map snd list) || checkShadowing h s


------------------------------------------------
--                   Renaming                 --
------------------------------------------------


-- Goes through each type of statment and renames where possible with the new name
renameVar :: VarName -> VarName -> [Statement] -> [Statement]
renameVar name newName [] = []
renameVar name newName ((Declaration (Primitive scope varType x mayExpr)):xs) = (Declaration (Primitive scope varType (mattchingString name newName x) (renameMaybeExpr name newName mayExpr))) : renameVar name newName xs

renameVar name newName ((Declaration (TLock x)):xs) = (Declaration (TLock (mattchingString name newName x))) : renameVar name newName xs
renameVar name newName ((Declaration (Array varType x size mayExpr)):xs) = (Declaration (Array varType (mattchingString name newName x) size (renameMaybeExpr name newName mayExpr))) : renameVar name newName xs 
renameVar name newName ((Declaration (String x expr)):xs) = (Declaration (String (mattchingString name newName x) expr)) : renameVar name newName xs

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
renameMaybeExpr :: VarName -> VarName -> Maybe Expr -> Maybe Expr
renameMaybeExpr name newName (Nothing) = Nothing
renameMaybeExpr name newName (Just expr) = Just (renameExpr name newName expr) 

-- Function compares name with a given string, and in case true then return the new name, otherwise return the default name
mattchingString :: VarName -> VarName -> VarName -> VarName
mattchingString name newName x 
   | x == name = newName
   | otherwise = x

-- Goes through each type of expression and renames where it is possible with the new name
renameExpr :: VarName -> VarName -> Expr -> Expr
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
renameExprList :: VarName -> VarName -> [Expr] -> [Expr]
renameExprList name newName [] = []
renameExprList name newName (expr:xs) = (renameExpr name newName expr) : renameExprList name newName xs

-- Goes through each type of expression and renames where it is possible with the new name
renameCond :: VarName -> VarName -> Condition -> Condition
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


extractTypeHashmap :: Hashmap -> VarName -> HashmapType
extractTypeHashmap (Scope list []) s
   | elem s (map snd list) = fst $ fromJust (find (\x -> (snd x == s)) list)
   | otherwise = error ("The variable has not been declared yet: " ++ s)
extractTypeHashmap (Scope list [h]) s
   | elem s (map snd list) = fst $ fromJust (find (\x -> (snd x == s)) list)
   | otherwise = extractTypeHashmap h s

getStringType :: HashmapType -> String
getStringType HInt = "Int"
getStringType HBool = "Bool"
getStringType HChar = "Char"
getStringType HLock = "Lock"
getStringType HString = "String"
getStringType (HArray HInt) = "Int[]"
getStringType (HArray HBool) = "Bool[]"
getStringType (HArray HChar) = "Char[]"

