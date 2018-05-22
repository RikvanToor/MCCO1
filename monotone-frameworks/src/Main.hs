module Main where

import Lexer
import Parser
import AttributeGrammar
import System.IO

import Data.Graph

-- Helper functies voor parsen van files en strings
parse :: String -> Program
parse = happy . alex

parseFile :: FilePath -> IO Program
parseFile f = parse <$> readFile f

--------------------------------------------------------------------------------
--                           1. Semantische functies                          --
--------------------------------------------------------------------------------

type ControlFlowGraph = Graph

{- Deze functie voegt labels toe aan de AST. sem_Program is een automatisch
 - gegenereerde functie van type Program -> T_Program, waar T_Program een type
 - is van Int -> (Int, Program'). Dit kan dus nog veranderen wanneer we meer toe
 - gaan voegen aan de semantiek van Program.
 -
 - De Int is de initiele waarde voor de programmapunten.
 -}
runStatic :: Program -> (ControlFlowGraph, Program')
runStatic = sem_Program

main :: IO ()
main =
  do
    hSetBuffering stdin LineBuffering

    program_raw <- getLine >>= parseFile

    let (cfg, program_labeled) = runStatic program_raw

    --  Debugging
    print program_labeled

    let (finals, init) = sem_Program' program_labeled

    putStrLn $ "Init state: " ++ show init

    putStrLn $ "Final states: " ++ show finals

    -- /Debugging

    return ()