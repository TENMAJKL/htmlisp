module Lexer where
import Data.Maybe (isJust)
import Control.Monad (MonadPlus(mplus))

data Token = 
    HtmlString String
    | HtmlNode (String, [Token])
    deriving (Show)

lexChar :: Char -> String -> Maybe (Char, String)
lexChar target input = case input of
    (c:cs) | c == target -> Just (target, cs)
    _ -> Nothing

lexString :: String -> Maybe (Token, String)
lexString target = 
    lexChar '"' target 
    >>= (\(c, text) -> return (toStringResult (break (== '"') text)))

toStringResult :: (String, String) -> (Token, String)
toStringResult (parsed, next) = (HtmlString parsed, tail next)

lexNode :: String -> Maybe (Token, String)
lexNode target = 
    lexChar '(' target
    >>= (\(c, text) -> return (break (== ' ') text))
    >>= (\(name, next_str) -> case lexArguments next_str of 
        (Just (arguments, next)) -> return (HtmlNode (name, arguments), next)
        Nothing -> Nothing
    )

lexArguments :: String -> Maybe ([Token], String)
lexArguments target  
    | isJust (lexChar ')' target) = Just ([], tail target)
    | otherwise =
            lexChar ' ' target
            >>= (
                \(prev, next) -> 
                    case lexString next of
                        (Just string) -> return string
                        Nothing -> lexNode next
            )
        >>= (\(argument, next) -> 
            let Just (args, next_string) = lexArguments next in 
                return (argument : args, next_string))

