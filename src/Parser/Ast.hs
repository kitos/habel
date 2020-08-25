{-# LANGUAGE LambdaCase #-}
module Parser.Ast where

import Data.List
import Data.Bool

data Identifier = Identifier String

instance Show Identifier where
  show (Identifier s) = toJsonObj [
    ("type", show "Identifier"),
    ("name", show s)]

data ObjectProperty = ObjectProperty Identifier Expression

instance Show ObjectProperty where
  show (ObjectProperty k v) = toJsonObj [
    ("type", show "ObjectProperty"),
    ("key", show k),
    ("value", show v)]

data Expression =
  StringLiteral String
  | NumericLiteral Double
  | BoolLiteral Bool
  | ArrayLiteral [Expression]
  | ObjectLiteral [ObjectProperty]
  | FunctionCall Identifier [Expression]
  | Id Identifier

instance Show Expression where
  show = toJsonObj . (\case
    (StringLiteral v) -> [
      ("type", show "StringLiteral"),
      ("value", show v)]
    (NumericLiteral v) -> [
      ("type", show "NumericLiteral"),
      ("value", show v)]
    (BoolLiteral v) -> [
      ("type", show "BoolLiteral"),
      ("value", bool "false" "true" v)]
    (ArrayLiteral v) -> [
      ("type", show "ArrayLiteral"),
      ("elements", show v)]
    (ObjectLiteral v) -> [
      ("type", show "ObjectLiteral"),
      ("properties", show v)]
    (FunctionCall n a) -> [
      ("type", show "FunctionCall"),
      ("name", show n),
      ("args", show a)]
    (Id s) -> [
      ("type", show "Identifier"),
      ("name", show s)])

data VarType = Var | Let | VConst deriving (Show)

data Statement =
    VarDeclaration VarType Identifier Expression
  | FunctionDeclaration Identifier [Identifier] [Statement]
  | ExpressionStatement Expression

wrap (b, e) s = b:s ++ [e]

toJsonProp (k, v) = show k ++ ":" ++ v

toJsonObj = wrap ('{', '}') . intercalate "," . fmap toJsonProp

instance Show Statement where
  show = toJsonObj . (\case
    (VarDeclaration k i e) -> [
      ("type", show "VarDeclaration"),
      ("kind", show $ show k),
      ("left", show i),
      ("right", show e)]
    (FunctionDeclaration n a b) -> [
      ("type", show "FunctionDeclaration"),
      ("name", show n),
      ("args", show a),
      ("body", show b)]
    (ExpressionStatement e) -> [
      ("type", show "ExpressionStatement"),
      ("expr", show e)])

data Program = Program [Statement]

instance Show Program where
  show (Program xs) = toJsonObj [
    ("type", show "Program"),
    ("body", show xs)]
