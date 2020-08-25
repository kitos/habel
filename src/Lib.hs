{-# LANGUAGE LambdaCase, MultiWayIf #-}
module Lib where

import Control.Applicative
import Data.Foldable
import Parser.Parser
import Parser.Ast
import Parser.Core
import Parser.StringLiteral
import Parser.NumericLiteral
import Parser.BoolLiteral
import Parser.Identifier

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
    Id <$> parseId,
    inParens parseExpression]

parseExpressionStatement :: Parser Statement
parseExpressionStatement = ExpressionStatement <$> parseExpression

parseStatement = asum [
    parseVarDeclaration,
    parseFunctionDeclaration,
    parseExpressionStatement]

parseProgram :: Parser Program
parseProgram = Program <$> many (inWhitespaces parseStatement)
