module Main where

import Parser.Parser
import Lib

main = interact (show . runParser parseProgram)
