module Lib where

import Parser

data Identifier = Identifier String

data Args = Args [Identifier]

data Expr
    = FunctionDef Identifier Args [Expr]
    | FunctionCall Identifier [Expr]

-- parse1 :: (Char -> Bool) -> Parser Char

expect1 :: Char -> Parser Char
expect1 c = Parser(\s -> case s of
    []     -> Left (0, "Expected 1 character :-(")
    (x:xs) -> case x == c of
        True  -> Right (0, xs, c)
        False -> Left (1, "Expected " ++ [c] ++ ", got " ++ [x] ++ " :-("))



expect :: String -> Parser String
expect = sequence . fmap expect1

-- parseIdentifier :: Parser Identifier
-- parseIdentifier = 