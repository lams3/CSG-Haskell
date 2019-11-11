module Main where

import System.IO

import CSG
import CSG.Parser

main = do
    contents <- readFile "testFiles/Test.csg"
    case parse contents of
        Nothing -> putStrLn "Couldn't parse Test.csg"
        Just s -> print s
