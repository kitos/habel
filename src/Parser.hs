{-# LANGUAGE BangPatterns #-}
module Parser where

type ParserResult x = Either (Integer, String) (Integer, String, x)

data Parser x = Parser (String -> ParserResult x)

runParser :: Parser x -> String -> ParserResult x
runParser (Parser f) = f

instance Functor Parser where
    fmap f pa = pure f <*> pa

instance Applicative Parser where
    pure x = Parser (\s -> Right (0, s, x))

    p1 <*> p2 = p1 >>= \f -> p2 >>= \x -> pure $ f x

instance Monad Parser where
    p1 >>= f = Parser(\s0 ->
        case runParser p1 s0 of
            Left e -> Left e
            Right (!n0, s1, a) -> case runParser (f a) s1 of
                Left (!n1, msg) -> Left (n0 + n1, msg)
                Right (!n1, s2, b) -> Right (n0 + n1, s2, b))