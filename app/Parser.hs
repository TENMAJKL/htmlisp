module Parser where

import Lexer (Token(..))

parse :: Token -> Maybe (String)
parse token = case token of
    (HtmlString content) -> return content
    (HtmlNode (name, children)) -> 
        case parseChildren children of 
            (Just parsed) -> return ("<" ++ name ++ ">"  ++ parsed ++ "</" ++ name ++ ">")
            Nothing -> Nothing

parseChildren :: [Token] -> Maybe String
parseChildren [] = Just ""
parseChildren (c:cs) = parse c >>= (\parsed -> 
    case parseChildren cs of
        (Just children) -> return (parsed ++ children)
        Nothing -> Nothing
    )
