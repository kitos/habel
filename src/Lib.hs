{-# LANGUAGE LambdaCase, MultiWayIf #-}
module Lib where

import Control.Applicative
import Data.Foldable
import Parser
import Core

data Identifier = Identifier String
    deriving (Show)

data Expression = FunctionCall Identifier [Expression]
    | Id Identifier
    deriving (Show)

data VarType = Var | Let | VConst deriving (Show)

data Statement =
      VarDeclaration VarType Identifier Expression
    | FunctionDeclaration Identifier [Identifier] [Statement]
    | ExpressionStatement Expression
    deriving (Show)

data Program = Program [Statement] deriving (Show)

validIdStartChars = "_" ++ ['a'..'z'] ++ ['A'..'Z']
validIdChars = validIdStartChars ++ ['0'..'9']

parseIdStart = parse1 (\c -> case c `elem` validIdStartChars of
    True  -> Right c
    False -> Left "Expected identifier to start with '_' or 'a'..'z' :(")

parseIdTail = parse1 (\c -> case c `elem` validIdChars of
    True  -> Right c
    False -> Left ("Using " <> show c <> " in identifier is not allowed"))

parseId :: Parser Identifier
parseId = (\s0 s -> Identifier (s0:s)) <$> parseIdStart <*> (many parseIdTail)

separatedByCharWithSpaces = separatedBy . inWhitespaces . expect1

parseFunctionDeclaration :: Parser Statement
parseFunctionDeclaration = expect "function" >>
    whitespaces >>
    parseId >>= \n ->
    whitespaces >>
    inParens (separatedByCharWithSpaces ',' parseId) >>= \args ->
    whitespaces >>
    inCurlyBrackets (many parseStatement) >>= \ss ->
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
    (inWhitespaces . expect1 $ '=') >>
    parseExpression >>= \e ->
    return (VarDeclaration t n e)

parseFunctionCall :: Parser Expression
parseFunctionCall = parseId >>= \n ->
    whitespaces >>
    (inParens . inWhitespaces . separatedByCharWithSpaces ',' $ parseExpression) >>= \args ->
    return (FunctionCall n args)

parseExpression :: Parser Expression
parseExpression = asum [parseFunctionCall, Id <$> parseId]

parseExpressionStatement :: Parser Statement
parseExpressionStatement = ExpressionStatement <$> parseExpression

parseStatement = asum [
    parseVarDeclaration,
    parseFunctionDeclaration,
    parseExpressionStatement]

parseProgram :: Parser Program
parseProgram = Program <$> many (inWhitespaces parseStatement)
