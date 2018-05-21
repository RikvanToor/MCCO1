module Main where

import Lexer
import Parser
import AttributeGrammar

import System.Directory
import System.IO

-- Helper functies voor parsen van files en strings
parse :: String -> Program
parse = happy . alex

parseFile :: FilePath -> IO Program
parseFile f = parse <$> readFile f

--------------------------------------------------------------------------------
--                           1. Semantische functies                          --
--------------------------------------------------------------------------------

{- Deze functie voegt labels toe aan de AST. sem_Program is een automatisch
 - gegenereerde functie van type Program -> T_Program, waar T_Program een type
 - is van Int -> (Int, Program'). Dit kan dus nog veranderen wanneer we meer toe
 - gaan voegen aan de semantiek van Program.
 -
 - De Int is de initiele waarde voor de programmapunten.
 -}
labeler :: Program -> Program'
labeler p =
  let (_, program_labeled) = sem_Program p 1
  in program_labeled

main :: IO ()
main =
  do
    hSetBuffering stdin LineBuffering

    program_raw <- getLine >>= parseFile

    let program_labeled = labeler program_raw

    --  Debugging
    print program_labeled

    -- /Debugging

    return ()