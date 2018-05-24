module Test where

import Lexer
import Parser
import AttributeGrammar
import System.Directory

-- Parsen
parse :: String -> Program
parse = happy . alex

parseFile :: FilePath -> IO Program
parseFile f = parse <$> readFile f

-- Testen
testDir :: FilePath
testDir = "../examples/"

testFiles :: IO [FilePath]
testFiles = listDirectory testDir

runAllTestFiles :: IO ()
runAllTestFiles =
  do
    cs <- testFiles
    let fs = fmap (testDir ++) cs

    ps <- mapM parseFile fs
    let tested = [uncurry test prog | prog <- zip cs ps, test <- allTests]
    mapM_ putStrLn tested

-- Specifieke tests
allTests :: [FilePath -> Program -> String]
allTests =
  [ test_Labels
  , test_Init
  , test_Finals
  , test_Blocks
  , test_Flow
  , test_ReverseFlow
  ]

test_Labels :: FilePath -> Program -> String
test_Labels fp p =
  let (Program' _ s) = sem_Program p
  in report (fp ++ ": test_Labels") (show s)

test_Init :: FilePath -> Program -> String
test_Init fp p =
  let p' = sem_Program p
      (_, _, _, init, _) = sem_Program' p'
  in report (fp ++ ": test_Init ") (show init)

test_Finals :: FilePath -> Program -> String
test_Finals fp p =
  let p' = sem_Program p
      (_, _, finals, _, _) = sem_Program' p'
  in report (fp ++ ": test_Finals ") (show finals)

test_Blocks :: FilePath -> Program -> String
test_Blocks fp p =
  let p' = sem_Program p
      (blocks, _, _, _, _) = sem_Program' p'
  in report (fp ++ ": test_Blocks ") (show blocks)

test_Flow :: FilePath -> Program -> String
test_Flow fp p =
  let p' = sem_Program p
      (_, cfg, _, _, _) = sem_Program' p'
  in report (fp ++ ": test_Flow ") (show cfg)

test_ReverseFlow :: FilePath -> Program -> String
test_ReverseFlow fp p =
  let p' = sem_Program p
      (_, _, _, _, rcfg) = sem_Program' p'
  in report (fp ++ ": test_ReverseFlow ") (show rcfg)

-- Reportage
line :: String
line = replicate 80 '-'

report :: String -> String -> String
report header content =
  unlines
    [ line
    , header
    , ""
    , content
    ]
