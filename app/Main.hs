module Main where
import Lexer (lexNode)
import Parser (parse)
import System.Environment (getArgs)
import Data.String (String)
import Data.Maybe (Maybe(Nothing, Just))
import GHC.Base (IO)
import Prelude

transpile :: String -> String
transpile lisp = case lexNode lisp >>= (\(token, next) -> parse token) of 
    (Just result) -> result
    Nothing-> "Error"

parseArgs :: [String] -> IO () 
parseArgs [target, result] = do
    content <- readFile target
    writeFile result (transpile content)

parseArgs _ = putStrLn "Error"

main :: IO ()
main = do
    args <- getArgs
    parseArgs args

