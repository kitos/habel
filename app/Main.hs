module Main where

import System.IO 
import Lib
import Parser

main = do
    handle <- openFile "./test/example.js" ReadMode
    contents <- hGetContents handle
    putStr . show . runParser parseProgram $ contents
    hClose handle  
