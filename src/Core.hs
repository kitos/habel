{-# LANGUAGE LambdaCase, MultiWayIf #-}
module Core where

import Control.Applicative
import Data.Foldable
import Parser

parse1 :: (Char -> Either String x) -> Parser x
parse1 f = Parser (\case
    []    -> Left (0, "Not enough input :(")
    (c:s) -> case f c of
        Left m  -> Left (1, m)
        Right x -> Right (1, s, x))

expect1 :: Char -> Parser Char
expect1 x = parse1 (\c -> if
    | c == x    -> Right c
    | otherwise -> Left ("Expected " <> show x <> ", got " <> show c <> " :("))

notExpect1 :: Char -> Parser Char
notExpect1 x = parse1 (\c -> if
    | c /= x    -> Right c
    | otherwise -> Left ("Didn't expect to  get " <> show c <> " :("))

parseUntilChar = many . notExpect1

expect :: String -> Parser String
expect = sequence . fmap expect1

wrappedIn :: (Parser a, Parser b) -> Parser x -> Parser x
wrappedIn (pre, post) pa = pre >> pa >>= \x -> post >> return x

wrappedInChar :: (Char, Char) -> Parser x -> Parser x
wrappedInChar (pre, post) = wrappedIn (expect1 pre, expect1 post)

separatedBy :: Parser s -> Parser x -> Parser [x]
separatedBy s pa = pure (:) <*> pa <*> many (s *> pa)

separatedByChar :: Char -> Parser x -> Parser [x]
separatedByChar c = separatedBy (expect1 c)

whitespace = asum (expect1 <$> " \n\t\r")

whitespaces = many whitespace

inParens = wrappedInChar ('(', ')')

inBrackets = wrappedInChar ('[', ']')

inCurlyBrackets = wrappedInChar ('{', '}')

inWhitespaces = wrappedIn (whitespaces, whitespaces)
