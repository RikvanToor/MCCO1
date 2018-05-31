module Main where

import Lexer
import Parser
import AttributeGrammar
import Test
import Data.Graph

main :: IO ()
main = runAllTestFiles Nothing