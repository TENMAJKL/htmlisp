module Main where
import Lexer (lexNode)
import Parser (parse)

transpile :: String -> String
transpile lisp = case lexNode lisp >>= (\(token, next) -> parse token) of 
    (Just result) -> result
    Nothing -> "Error"

main :: IO ()
main = print (transpile "(html (head (title \"cs\")) (body \"cs\"))")

