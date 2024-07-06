import MyParser
import Test.Hspec
import Test.QuickCheck
import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Data.Either

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

