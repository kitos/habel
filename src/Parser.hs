{-# LANGUAGE BangPatterns #-}
module Parser where

import Control.Applicative

type ParserResult x = Either (Integer, String) (Integer, String, x)

data Parser x = Parser (String -> ParserResult x)

runParser :: Parser x -> String -> ParserResult x
runParser (Parser f) = f

instance Functor Parser where
    fmap f pa = pure f <*> pa

instance Applicative Parser where
    pure x = Parser (\s -> Right (0, s, x))

    p1 <*> p2 = do
        f <- p1
        x <- p2
        return (f x)

instance Alternative Parser where
    empty = Parser (const (Left (0, "empty")))

    pa <|> pb = Parser (\s0 -> case runParser pa s0 of
        Right r -> Right r
        Left _ -> runParser pb s0)

instance Monad Parser where
    pa >>= f = Parser(\s0 ->
        case runParser pa s0 of
            Left e -> Left e
            Right (!n0, s1, a) -> case runParser (f a) s1 of
                Left (!n1, msg) -> Left (n0 + n1, msg)
                Right (!n1, s2, b) -> Right (n0 + n1, s2, b))
