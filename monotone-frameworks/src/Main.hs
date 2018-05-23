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


main :: IO ()
main =
  do
    hSetBuffering stdin LineBuffering

    program_raw <- getLine >>= parseFile

    let program_labeled = sem_Program program_raw

    --  Debugging
    print program_labeled

    let (cfg, finals, init, rcfg) = sem_Program' program_labeled

    putStrLn $ "Init state: " ++ show init

    putStrLn $ "Final states: " ++ show finals

    putStrLn $ "Control Flow Graph: " ++ show cfg
    putStrLn $ "Reverse Control Flow Graph: " ++ show rcfg

    -- /Debugging

    return ()