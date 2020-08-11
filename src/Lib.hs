{-# LANGUAGE LambdaCase, MultiWayIf #-}
module Lib where

import Control.Applicative
import Parser
import Core

data Identifier = Identifier String
    deriving (Show)

data FunctionDef = FunctionDef Identifier [Identifier] -- [Expr]
    deriving (Show)

data Expr = FunctionCall Identifier [Expr]
    deriving (Show)

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

parseFunctionDef :: Parser FunctionDef
parseFunctionDef = expect "function" >>
    expect1 ' ' >>
    parseId >>= \n ->
    inParens (separatedBy ',' parseId) >>= \args ->
    expect1 ' ' >>
    inCurlyBrackets (expect1 ' ') >>
    return (FunctionDef n args)
