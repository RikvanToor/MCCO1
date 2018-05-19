module Main where

import Lexer
import Parser
import AttributeGrammar

-- Helper functies voor parsen van files en strings
parse :: String -> Program
parse = happy . alex

parseFile :: FilePath -> IO Program
parseFile f =
  do
    c <- readFile f
    return (parse c)

main :: IO ()
main = undefined