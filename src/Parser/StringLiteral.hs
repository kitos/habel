module Parser.StringLiteral (parseStringLiteral) where

import Control.Applicative
import Parser.Core
import Parser.Ast

parseSingleQuoteString = expect1 '\'' *> parseUntilChar '\'' <* expect1 '\''

parseDoubleQuoteString = expect1 '"' *> parseUntilChar '"' <* expect1 '"'

parseStringLiteral = StringLiteral <$> (parseSingleQuoteString <|> parseDoubleQuoteString)