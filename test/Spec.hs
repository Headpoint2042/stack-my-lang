-- module Spec where

import MyParser
import MyCodeGen
import qualified Sprockell

import Test.Hspec
import Test.QuickCheck

import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Data.Either
import GHC.IO.Handle
import System.IO
import System.Directory

-- `shouldSatisfy` isLeft -> means an error was returned

main :: IO ()
main = hspec $ do
  describe "Parsing" $ do
    -- it "should parse numbers" $ do
    --     property $ \n -> (parseMyLang $ show (getPositive n)) `shouldBe` (Right (getPositive n) :: Either String Integer)
    it "parses a constant" $ do
      parse parseConst "" "42" `shouldBe` Right (Const 42)
      parse parseConst "" "1234567890" `shouldBe` Right (Const 1234567890)
      parse parseConst "" "abc" `shouldSatisfy` isLeft

    it "parses a variable" $ do
      parse var "" "x" `shouldBe` Right (Var "x")
      parse var "" "x3" `shouldBe` Right (Var "x3")
      parse var "" "variable" `shouldBe` Right (Var "variable")
      parse var "" "ValidName" `shouldBe` Right (Var "ValidName")
      parse var "" "0invalid" `shouldSatisfy` isLeft

    it "parses true/false" $ do
      parse boolean "" "true" `shouldBe` Right (Boolean True)
      parse boolean "" "false" `shouldBe` Right (Boolean False)
      parse boolean "" "abc" `shouldSatisfy` isLeft
      parse boolean "" "123" `shouldSatisfy` isLeft
 
    it "parses a string literal" $ do
      parse parseString "" "\"Hello\"" `shouldBe` Right (StringLiteral "Hello")

    it "parses an addition expression" $ do
      parse expr "" "1 + 2" `shouldBe` Right (Add (Const 1) (Const 2))

    it "parses a subtraction expression" $ do
      parse expr "" "3 - 1" `shouldBe` Right (Sub (Const 3) (Const 1))

    it "parses a multiplication expression" $ do
      parse expr "" "2 * 3" `shouldBe` Right (Mult (Const 2) (Const 3))

    it "parses a division expression" $ do
      parse expr "" "6 / 2" `shouldBe` Right (Div (Const 6) (Const 2))

    it "parses a declaration" $ do
      parse declaration "" "int x = 5;" `shouldBe` Right (Primitive Local TInt "x" (Just (Const 5)))

    it "parses an assignment" $ do
      parse assignment "" "x = 10" `shouldBe` Right (Absolute "x" (Const 10))

    it "parses a block" $ do
      parse block "" "{ int x = 5; x = 10; }" `shouldBe` Right [Block [ Declaration (Primitive Local TInt "x" (Just (Const 5)))
                                                               , Assignment (Absolute "x" (Const 10)) ]]

    it "parses a program" $ do
      parse program "" "int x = 5; x = 10; " `shouldBe` Right (Program [ Declaration (Primitive Local TInt "x" (Just (Const 5)))
                                                                         , Assignment (Absolute "x" (Const 10)) ]) 
                                                                         
    it "parses a condition" $ do
      parse condition "" "true && false" `shouldBe` Right (And (Boolean True) (Boolean False))
      parse condition "" "true || false" `shouldBe` Right (Or (Boolean True) (Boolean False))
      parse condition "" "true && false" `shouldBe` Right (And (Boolean True) (Boolean False))
      parse condition "" "2 <= 3" `shouldBe` Right (Le (Expr (Const 2)) (Expr (Const 3)))
      parse condition "" "x >= 5" `shouldBe` Right (Ge (Expr (Var "x")) (Expr (Const 5)))
      parse condition "" "34 < y" `shouldBe` Right (Lt (Expr (Const 34)) (Expr (Var "y")))
      parse condition "" "2 > 5" `shouldBe` Right (Gt (Expr (Const 2)) (Expr (Const 5)))
      parse condition "" "x == 2" `shouldBe` Right (Eq (Expr (Var "x")) (Expr (Const 2)))
      parse condition "" "3 != 5" `shouldBe` Right (Neq (Expr (Const 3)) (Expr (Const 5)))


  -------------------------------------------
  --               COMPILING               --
  -------------------------------------------

  describe "Compiling" $ do
    it "prints a number" $ do
      output <- runFile "printConst"
      output `shouldBe` "Sprockell 0 says 101\n"

    it "prints a character" $ do
      output <- runFile "printChar"
      output `shouldBe` "Sprockell 0 says a\n"

    it "prints a boolean" $ do
      let expectedLines = [ "Sprockell 0 says 1"
                          , "Sprockell 0 says 0"
                          ]
      output <- runFile "printBool"
      lines output `shouldBe` expectedLines

    it "prints nested variables" $ do
      let expectedLines = [ "Sprockell 0 says 10"
                          , "Sprockell 0 says W"
                          , "Sprockell 0 says 0"
                          , "Sprockell 0 says 1"
                          ]
      output <- runFile "printVars"
      lines output `shouldBe` expectedLines

    it "prints sum of two numbers" $ do
      let expectedLines = [ "Sprockell 0 says 3"
                          , "Sprockell 0 says 3"
                          ]
      output <- runFile "addTwoNums"
      lines output `shouldBe` expectedLines

    -- TODO: implement div for areaTriangle.txt
    it "prints squared area of a triangle" $ do
      let expectedLines = [ "Sprockell 0 says 216" ]
      output <- runFile "areaTriangle"
      lines output `shouldBe` expectedLines

    it "swaps two variables" $ do
      let expectedLines = [ "Sprockell 0 says 10"
                          , "Sprockell 0 says 5"
                          ]
      output <- runFile "swapVars"
      lines output `shouldBe` expectedLines

    it "tests boolean operations" $ do
      let expectedLines = [ "Sprockell 0 says 0"
                          , "Sprockell 0 says 0"
                          , "Sprockell 0 says 0"
                          , "Sprockell 0 says 1"
                          , "Sprockell 0 says 1"
                          , "Sprockell 0 says 1"
                          , "Sprockell 0 says 1"
                          , "Sprockell 0 says 1"
                          , "Sprockell 0 says 0"
                          , "Sprockell 0 says 1"
                          , "Sprockell 0 says 202"
                          , "Sprockell 0 says 404"
                          , "Sprockell 0 says 505"
                          , "Sprockell 0 says 606"
                          , "Sprockell 0 says 999"
                          ]
      output <- runFile "logicalOps"
      lines output `shouldBe` expectedLines

    it "tests conditionals" $ do
      let expectedLines = [ "Sprockell 0 says 1"
                          , "Sprockell 0 says 3"
                          , "Sprockell 0 says 6"
                          , "Sprockell 0 says 8"
                          , "Sprockell 0 says 10"
                          , "Sprockell 0 says 12"
                          , "Sprockell 0 says 14"
                          , "Sprockell 0 says 17"
                          , "Sprockell 0 says 18"
                          , "Sprockell 0 says 20"
                          , "Sprockell 0 says 24"
                          , "Sprockell 0 says 25"
                          , "Sprockell 0 says 28"
                          , "Sprockell 0 says 30"
                          , "Sprockell 0 says 32"
                          , "Sprockell 0 says 33"
                          ]
      output <- runFile "testConditionals"
      lines output `shouldBe` expectedLines

    it "tests blocks/scopes" $ do
      let expectedLines = [ "Sprockell 0 says c"
                          , "Sprockell 0 says 10"
                          , "Sprockell 0 says 1"
                          , "Sprockell 0 says 6"
                          , "Sprockell 0 says 1"
                          , "Sprockell 0 says 7"
                          , "Sprockell 0 says 8"
                          , "Sprockell 0 says 9"
                          , "Sprockell 0 says 10"
                          , "Sprockell 0 says 20"
                          , "Sprockell 0 says 1"
                          , "Sprockell 0 says 19"
                          , "Sprockell 0 says 18"
                          , "Sprockell 0 says 17"
                          , "Sprockell 0 says 16"
                          , "Sprockell 0 says 15"
                          , "Sprockell 0 says 999"
                          ]
      output <- runFile "blocks"
      lines output `shouldBe` expectedLines

    it "tests if and while" $ do
      let expectedLines = [ "Sprockell 0 says 1"
                          , "Sprockell 0 says 3"
                          , "Sprockell 0 says 5"
                          , "Sprockell 0 says 5"
                          , "Sprockell 0 says 6"
                          , "Sprockell 0 says 7"
                          , "Sprockell 0 says 7"
                          , "Sprockell 0 says 0"
                          , "Sprockell 0 says 1"
                          , "Sprockell 0 says 0"
                          , "Sprockell 0 says 1"
                          , "Sprockell 0 says 0"
                          , "Sprockell 0 says 1"
                          , "Sprockell 0 says 999"
                          ]
      output <- runFile "checkIfWhile"
      lines output `shouldBe` expectedLines

    -- this test takes more than others (around 1.5 sec) because of division of big numbers (1000000 / 3)
    it "tests arithmetic operations (extensive)" $ do
      let expectedLines = [ "Sprockell 0 says 3"
                          , "Sprockell 0 says 2"
                          , "Sprockell 0 says 6"
                          , "Sprockell 0 says 3"
                          , "Sprockell 0 says 7"
                          , "Sprockell 0 says 9"
                          , "Sprockell 0 says 1"
                          , "Sprockell 0 says 65"
                          , "Sprockell 0 says 10"
                          , "Sprockell 0 says 10"
                          , "Sprockell 0 says -3"
                          , "Sprockell 0 says 0"
                          , "Sprockell 0 says 2000000"
                          , "Sprockell 0 says 1000000"
                          , "Sprockell 0 says 333333"
                          , "Sprockell 0 says 0"
                          , "Sprockell 0 says 0"
                          , "Sprockell 0 says 3"
                          , "Sprockell 0 says -2"
                          , "Sprockell 0 says 105"
                          , "Sprockell 0 says 68"
                          , "Sprockell 0 says 0"
                          ]
      output <- runFile "testArithmetic"
      lines output `shouldBe` expectedLines

    

{-
it "swaps two variables" $ do
      let expectedLines = [ "Sprockell 0 says 10"
                          , "Sprockell 0 says 5"
                          ]
      output <- runFile "swapVars"
      lines output `shouldBe` expectedLines
-}

-- HELPER FUNCTIONS

-- path to programs directory
programsDir :: FilePath
programsDir = "./test/programs/"

-- string to hold the file extension
fileExt :: String
fileExt = ".txt"

-- runFile - compile file and catch output
runFile :: FilePath -> IO String
runFile path = catchOutput $ compile (programsDir ++ path ++ fileExt)

-- function to compile files
compile :: FilePath -> IO ()
compile filePath = do
    input <- readFile filePath
    let output = createAST input
    case output of
        -- error
        Left  err -> do
            print err

        -- compile ast
        Right ast -> do
            let env = compileProgram ast
            let threads = mainCode env : threadsCode env
            -- putStrLn $ "Main Code: " ++ show (mainCode env)
            -- putStrLn $ "Threads code: " ++ show (threadsCode env)
            Sprockell.run threads

-- capture the output of an IO action that writes to "stdout"
catchOutput :: IO () -> IO String
catchOutput f = do
  tmpd <- getTemporaryDirectory
  (tmpf, tmph) <- openTempFile tmpd "haskell_stdout"
  stdout_dup <- hDuplicate stdout
  hDuplicateTo tmph stdout
  hClose tmph
  f
  hFlush stdout
  hDuplicateTo stdout_dup stdout
  hClose stdout_dup
  -- str <- readFile tmpf
  -- removeFile tmpf      -- when removing the file an exception is thrown because it is still being read (idk how to fix this)
  -- return str
  readFile tmpf