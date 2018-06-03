module Test where

import           AttributeGrammar
import           Data.List
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
                Just bl -> filter (`notElem` (bl ++ fmap (testDir++) bl)) fs'
                _       -> []

    ps <- mapM parseFile fs
    let tested = [uncurry test prog | prog <- zip fs ps, test <- allTests]
    mapM_ putStrLn tested

type Test = FilePath -> Program -> String

runTest :: Test -> FilePath -> IO String
runTest t f =
  do
    p <- parseFile (testDir ++ f)
    return (t f p)

runTestPrint :: Test -> FilePath -> IO ()
runTestPrint t f = runTest t f >>= putStrLn

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
  , test_CP
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


-- Constant Propagation

test_CP :: FilePath -> Program -> String
test_CP fp p =
  let res = sem_Program' . sem_Program $ p
      monotoneFrameworkInstance =
        let u = CP . M.fromList $ [(var, D Top) | var <- agResult_all_vars res]
        in mkMFInstance
            u -- Hetzelfde als top
            (agResult_cfg res)
            (agResult_blocks res)
            [agResult_init res]
            u -- \x -> Top voor elke variabele

      rep = unlines . fmap presentAnalysis_CP . M.toList . maximalFixedPoint $ monotoneFrameworkInstance

  in report (fp ++ ": test_ConstantPropagation") (rep)


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

data Align = L | R

padWith :: Char -> Int -> Align -> String -> String
padWith c n align str =
  let padding = replicate (n - length str) c
  in case align of
    L -> str ++ padding
    R -> padding ++ str

presentAnalysis_CP :: (Label, Analysis_CP) -> String
presentAnalysis_CP (l, (CP x)) =
  let
    varmap =
      intercalate ", " . fmap (\(var, val) -> concat [var, "=", show val]) $ M.toList x
  in show l ++ "\t{" ++ varmap ++ "}"

