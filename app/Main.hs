module Main where

import System.IO 
import Lib
import Parser

main = interact $ show . runParser parseProgram
