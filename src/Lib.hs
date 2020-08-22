{-# LANGUAGE LambdaCase, MultiWayIf #-}
module Lib where

import Control.Applicative
import Data.Foldable
import Parser.Parser
import Parser.Ast
import Core

parseSingleQuoteString = expect1 '\'' *> parseUntilChar '\'' <* expect1 '\''

parseDoubleQuoteString = expect1 '"' *> parseUntilChar '"' <* expect1 '"'

parseStringLiteral = StringLiteral <$> (parseSingleQuoteString <|> parseDoubleQuoteString)

digits = ['0'..'9']

parseDigit = parse1 (\c -> case c `elem` digits of
    True  -> Right c
    False -> Left ("Expected '0'..'9', got" <> show c <> " :("))

parseNumericLiteral = NumericLiteral . read <$> some parseDigit

parseBool = (const True <$> expect "true") <|> (const False <$> expect "false")

parseBoolLiteral = BoolLiteral <$> parseBool

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

separatedByCharWithSpaces = separatedBy . inWhitespaces . expect1

parseArrayLiteral :: Parser Expression
parseArrayLiteral = ArrayLiteral <$> inBrackets (separatedByCharWithSpaces ',' parseExpression)

parseObjectProperty :: Parser ObjectProperty
parseObjectProperty = parseId >>= \i ->
    inWhitespaces (expect1 ':') >>
    parseExpression >>= \e ->
    return (ObjectProperty i e)

parseObjectLiteral :: Parser Expression
parseObjectLiteral = ObjectLiteral <$> (
    inCurlyBrackets . inWhitespaces . separatedByCharWithSpaces ',' $ parseObjectProperty)

parseFunctionDeclaration :: Parser Statement
parseFunctionDeclaration = expect "function" >>
    whitespaces >>
    parseId >>= \n ->
    whitespaces >>
    inParens (separatedByCharWithSpaces ',' parseId) >>= \args ->
    whitespaces >>
    (inCurlyBrackets . many . inWhitespaces $ parseStatement) >>= \ss ->
    return (FunctionDeclaration n args ss)

parseVarType :: Parser VarType
parseVarType = asum [
    const Let   <$> expect "let",
    const VConst <$> expect "const",
    const Var   <$> expect "var"
    ]

parseVarDeclaration :: Parser Statement
parseVarDeclaration = parseVarType >>= \t ->
    whitespaces >>
    parseId >>= \n ->
    inWhitespaces (expect1 '=') >>
    parseExpression >>= \e ->
    return (VarDeclaration t n e)

parseFunctionCall :: Parser Expression
parseFunctionCall = parseId >>= \n ->
    whitespaces >>
    (inParens . inWhitespaces . separatedByCharWithSpaces ',' $ parseExpression) >>= \args ->
    return (FunctionCall n args)

parseExpression :: Parser Expression
parseExpression = asum [
    parseBoolLiteral,
    parseNumericLiteral,
    parseStringLiteral,
    parseArrayLiteral,
    parseObjectLiteral,
    parseFunctionCall,
    Id <$> parseId]

parseExpressionStatement :: Parser Statement
parseExpressionStatement = ExpressionStatement <$> parseExpression

parseStatement = asum [
    parseVarDeclaration,
    parseFunctionDeclaration,
    parseExpressionStatement]

parseProgram :: Parser Program
parseProgram = Program <$> many (inWhitespaces parseStatement)
