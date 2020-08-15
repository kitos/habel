module Wasm where

import Asterius.Types
import Lib
import Parser

parseSrc = toJSString . show . runParser parseProgram . fromJSString

foreign export javascript "parseSrc" parseSrc :: JSString -> JSString
