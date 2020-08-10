module Main where

import Lib

main :: IO ()
main = do
    code <- getLine
    putStrLn $ show $ code
