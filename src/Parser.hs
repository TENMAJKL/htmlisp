module Parser where

parse :: Token -> string
parse token = case token of
    (HtmlString content) -> content
    (HtmlNode (name, children)) -> "<" ++ name ++ ">" ++ (foldl (\(c, i) -> (c ++ parse i)) ++ "</" ++ name ++ ">"
