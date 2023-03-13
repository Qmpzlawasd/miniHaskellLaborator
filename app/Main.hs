
module Main where

import System.IO
import System.Console.Isocline

import Exp
import Parsing
import Printing
import REPLCommand
import Text.ParserCombinators.Parsec 

main :: IO ()
main = do
    setHistory "history.txt" 200
    input <- readline "minHaskell"
    case parse replCommand "<input>" input of
        Left err -> print err >> main
        Right cmd -> case cmd of
                        Quit -> return ()
                        Load _ -> putStrLn("Not implemented") >> main
                        Eval s -> case parse exprParser "<input>" s of
                            Left err -> print err >> main
                            Right c -> putStrLn (showExp c) >> main