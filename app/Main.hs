module Main where

import MyParser
import MyElaborator
import MyCodeGen
import Sprockell

import System.Environment

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
            run threads


main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> compile filePath
        []         -> error $ "Please give a file path something!"