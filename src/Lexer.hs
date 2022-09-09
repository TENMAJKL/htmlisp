module Lexer where

data Token = 
    HtmlString String
    | HtmlNode (String, [Token])

lexChar :: Char -> String -> Maybe (Char, String)
lexChar target (c:cs) = case c of
    target -> Just (target, cs)
    _ -> Nothing

lexString :: String -> Maybe (Token, String)
lexString target = 
    lexChar '"' target 
    >>= (\(c, text) -> return (toStringResult (break (\c -> c == '"') text)))

toStringResult :: (String, String) -> (Token, String)
toStringResult (parsed, next) = (HtmlString parsed, tail next)

lexNode :: String -> Maybe (Token, String)
lexNode target = 
    lexChar '(' target
    >>= (\(c, text) -> return (break (\c -> c == ' ') text))
    >>= (\(name, next_str) -> 
        let (arguments, next) = (lexArguments next) in 
        return (HtmlNode(name, arguments), next))

lexArguments :: String -> Maybe ([Token], String)
lexArguments target  
    | isJust (lexChar ')' target) = ([], (tail target))
    | otherwise =
            lexChar ' ' target
            >>= (
                (\(prev, next) -> lexString next)
                `mplus` (\(prev, next) -> lexNode next)
            )
        >>= (\(argument, next) -> let (args, next_string) = lexArguments next in (argument : agrs, next_string))
