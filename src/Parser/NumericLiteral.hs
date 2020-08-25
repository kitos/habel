module Parser.NumericLiteral (parseNumericLiteral) where

import Control.Applicative
import Parser.Core
import Parser.Ast

digits = ['0'..'9']

parseDigit = parse1 (\c -> case c `elem` digits of
    True  -> Right c
    False -> Left ("Expected '0'..'9', got" <> show c <> " :("))

parseNumericLiteral = NumericLiteral . read <$> some parseDigit