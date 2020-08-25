module Parser.Identifier (parseId) where

import Control.Applicative
import Parser.Parser
import Parser.Core
import Parser.Ast

digits = ['0'..'9']

validIdStartChars = "_" ++ ['a'..'z'] ++ ['A'..'Z']
validIdChars = validIdStartChars ++ digits

parseIdStart = parse1 (\c -> case c `elem` validIdStartChars of
    True  -> Right c
    False -> Left "Expected identifier to start with '_' or 'a'..'z' :(")

parseIdTail = parse1 (\c -> case c `elem` validIdChars of
    True  -> Right c
    False -> Left ("Using " <> show c <> " in identifier is not allowed :("))

parseId :: Parser Identifier
parseId = (\s0 s -> Identifier (s0:s)) <$> parseIdStart <*> (many parseIdTail)