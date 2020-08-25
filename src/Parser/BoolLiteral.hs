module Parser.BoolLiteral (parseBoolLiteral) where

import Control.Applicative
import Parser.Core
import Parser.Ast

parseBool = (const True <$> expect "true") <|> (const False <$> expect "false")

parseBoolLiteral = BoolLiteral <$> parseBool