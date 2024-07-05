module Main where

import MyParser
import MyCodeGen
import Sprockell

-- Compiles a number into a spril program producing all fibonacci numbers below the number
-- Compilation might fail
-- compile :: String -> Either String [Instruction]
-- compile txt = do
--     ast <- parseMyLang txt
--     pure $ codeGen ast

-- Gets a number and runs the resulting spril program of compilation succeeds
-- main :: IO ()
-- main = do
--     txt <- getLine
--     putStrLn txt
    -- case compile txt of
    --     (Left err) -> fail err
    --     (Right spril) -> run [spril]

main :: IO ()
main = do
    args <- getArgs