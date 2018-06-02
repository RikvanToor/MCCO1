module Test where

import           AttributeGrammar
import           Data.Map (Map)
import           Data.Set (Set)
import           Lexer
import           MFPAlgorithm
import           Parser
import           System.Directory
import qualified Data.Map as M
import qualified Data.Set as S


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

runAllTestFiles :: Maybe [FilePath] -> IO ()
runAllTestFiles blacklist =
  do
    cs <- testFiles
    let fs' = fmap (testDir ++) cs
        fs  = case blacklist of
                Just bl -> filter (`notElem` bl) fs'
                _       -> []

    ps <- mapM parseFile fs
    let tested = [uncurry test prog | prog <- zip cs ps, test <- allTests]
    mapM_ putStrLn tested

type Test = FilePath -> Program -> String

runTest :: Test -> FilePath -> IO String
runTest t f =
  do
    p <- parseFile (testDir ++ f)
    return (t f p)

-- Specifieke tests
allTests :: [Test]
allTests =
  [ test_Labels
  , test_Init
  , test_Finals
  , test_Blocks
  , test_Flow
  , test_ReverseFlow
  , test_AExpr
  , test_SLV
  ]

test_Labels :: FilePath -> Program -> String
test_Labels fp p =
  let Program' _ s = sem_Program p
  in report (fp ++ ": test_Labels") (show s)

test_Init :: FilePath -> Program -> String
test_Init fp p =
  let res = agResult_init . sem_Program' . sem_Program $ p
  in report (fp ++ ": test_Init ") (show res)

test_Finals :: FilePath -> Program -> String
test_Finals fp p =
  let res = agResult_finals . sem_Program' . sem_Program $ p
  in report (fp ++ ": test_Finals ") (show res)

test_Blocks :: FilePath -> Program -> String
test_Blocks fp p =
  let res = agResult_blocks . sem_Program' . sem_Program $ p
  in report (fp ++ ": test_Blocks ") (show res)

test_Flow :: FilePath -> Program -> String
test_Flow fp p =
  let res = agResult_cfg . sem_Program' . sem_Program $ p
  in report (fp ++ ": test_Flow ") (show res)

test_ReverseFlow :: FilePath -> Program -> String
test_ReverseFlow fp p =
  let res = agResult_rcfg . sem_Program' . sem_Program $ p
  in report (fp ++ ": test_ReverseFlow ") (show res)

-- Available expressions

test_AExpr :: FilePath -> Program -> String
test_AExpr fp p =
  let res = sem_Program' . sem_Program $ p
      monotoneFrameworkInstance =
        mkMFInstance
          (AE . S.fromList . agResult_aexpr_star $ res)
          (agResult_cfg res)
          (agResult_blocks res)
          [agResult_init res]
          (AE S.empty)
      rep = fmap (show . S.toList . toSet) . M.elems . maximalFixedPoint $ monotoneFrameworkInstance
  in report (fp ++ ": test_AExpr ") (unlines rep)

-- Live variables

test_SLV :: FilePath -> Program -> String
test_SLV fp p =
  let res = sem_Program' .  sem_Program $ p
      monotoneFrameworkInstance =
        mkMFInstance
          (SLV . S.fromList . agResult_all_vars $ res)
          (agResult_rcfg res)
          (agResult_blocks res)
          (agResult_finals res)
          (SLV S.empty)
      rep = fmap (show . S.toList . toSet) . M.elems . maximalFixedPoint $ monotoneFrameworkInstance
  in report (fp ++ ": test_StronglyLiveVariables") (unlines rep)

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
