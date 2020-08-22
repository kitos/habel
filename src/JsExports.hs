module JsExports where

import Asterius.Types
import Lib
import Parser.Parser
import Parser.Ast

stringifyParserResult :: Show x => ParserResult x -> String
stringifyParserResult (Left (_, m)) = toJsonObj [("bullshit", show m)]
stringifyParserResult (Right (_, m, r)) = toJsonObj [("result", show r), ("bullshit", show m)]

parseSrc = toJSString . stringifyParserResult . runParser parseProgram . fromJSString

foreign export javascript "parseSrc" parseSrc :: JSString -> JSString
