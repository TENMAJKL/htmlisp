module Lexer where
import Data.Maybe (isJust)
import Control.Monad (MonadPlus(mplus))

data Token = 
    HtmlString String
    | HtmlNode (String, [Token])
    deriving (Show)

isWhite :: Char -> Bool
isWhite char = char == ' ' || char == '\n'

lexChar :: String -> String -> Maybe (Char, String)
lexChar target input = case input of
    (c:cs) | c `elem` target -> Just (c, cs)
    _ -> Nothing

lexString :: String -> Maybe (Token, String)
lexString target = 
    lexChar "\"" target 
    >>= (\(c, text) -> return (toStringResult (break (== '"') text)))

toStringResult :: (String, String) -> (Token, String)
toStringResult (parsed, next) = (HtmlString parsed, tail next)

lexNode :: String -> Maybe (Token, String)
lexNode target = 
    lexChar "(" target
    >>= (\(c, text) -> return (break isWhite text))
    >>= (\(name, next_str) -> case lexArguments (dropWhile isWhite next_str) of 
        (Just (arguments, next)) -> return (HtmlNode (name, arguments), next)
        Nothing -> Nothing
    )

lexArguments :: String -> Maybe ([Token], String)
lexArguments target =
        (
            case lexString target of
                (Just string) -> return string
                Nothing -> lexNode target
        )
        >>= (\(argument, next_str) ->
            let (_, next) = span isWhite next_str in
            case lexChar ")" next of
                (Just (_, string)) -> return ([argument], string)
                Nothing -> case lexArguments next of
                    Just (args, next_string) -> return (argument : args, next_string)
                    Nothing -> Nothing 
            )
