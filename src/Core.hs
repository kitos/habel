{-# LANGUAGE LambdaCase, MultiWayIf #-}
module Core where

import Control.Applicative
import Parser

parse1 :: (Char -> Either String x) -> Parser x
parse1 f = Parser (\case
    []    -> Left (0, "Not enough input :(")
    (c:s) -> case f c of
        Left m  -> Left (0, m)
        Right x -> Right (1, s, x))

expect1 :: Char -> Parser Char
expect1 x = parse1 (\c -> if
    | c == x    -> Right c
    | otherwise -> Left ("Expected " <> show x <> ", got " <> show c <> " :("))

expect :: String -> Parser String
expect = sequence . fmap expect1

wrapedIn :: (Char, Char) -> Parser x -> Parser x
wrapedIn (o, c) pa = expect1 o >> pa >>= \x -> expect1 c >> return x

inParens = wrapedIn ('(', ')')

inBrackets = wrapedIn ('[', ']')

inCurlyBrackets = wrapedIn ('{', '}')

separatedBy :: Char -> Parser x -> Parser [x]
separatedBy c pa = pure (:) <*> pa <*> many (expect1 c *> pa)
