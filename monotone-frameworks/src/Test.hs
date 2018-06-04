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

runAllTestFiles :: Int -> Maybe [FilePath] -> IO ()
runAllTestFiles k blacklist =
  do
    cs <- testFiles
    let fs' = fmap (testDir ++) cs
        fs  = case blacklist of
                Just bl -> filter (`notElem` (bl ++ fmap (testDir++) bl)) fs'
                _       -> fs'

    ps <- mapM parseFile fs
    let tested = [uncurry (test k) prog | prog <- zip fs ps, test <- allTests ]
    mapM_ putStrLn tested

type Test = Int -> FilePath -> Program -> String

runTest :: Test -> Int -> FilePath -> IO String
runTest t k f =
  do
    p <- parseFile (testDir ++ f)
    return (t k f p)

runTestPrint :: Test -> Int -> FilePath -> IO ()
runTestPrint t k f = runTest t k f >>= putStrLn

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

test_Labels :: Int ->  FilePath -> Program -> String
test_Labels _ fp p =
  let Program' _ s = sem_Program p
  in report (fp ++ ": test_Labels") (show s)

test_Init :: Int ->  FilePath -> Program -> String
test_Init _ fp p =
  let res = agResult_init . sem_Program' . sem_Program $ p
  in report (fp ++ ": test_Init ") (show res)

test_Finals :: Int ->  FilePath -> Program -> String
test_Finals _ fp p =
  let res = agResult_finals . sem_Program' . sem_Program $ p
  in report (fp ++ ": test_Finals ") (show res)

test_Blocks :: Int ->  FilePath -> Program -> String
test_Blocks _ fp p =
  let res = agResult_blocks . sem_Program' . sem_Program $ p
  in report (fp ++ ": test_Blocks ") (show res)

test_Flow :: Int ->  FilePath -> Program -> String
test_Flow _ fp p =
  let res = agResult_cfg . sem_Program' . sem_Program $ p
  in report (fp ++ ": test_Flow ") (show res)

test_ReverseFlow :: a ->  FilePath -> Program -> String
test_ReverseFlow _ fp p =
  let res = agResult_rcfg . sem_Program' . sem_Program $ p
  in report (fp ++ ": test_ReverseFlow ") (show res)

-- Available expressions

test_AExpr :: Int -> FilePath -> Program -> String
test_AExpr k fp p =
  let res = sem_Program' . sem_Program $ p
      monotoneFrameworkInstance =
        mkMFInstance
          (AE . S.fromList . agResult_aexpr_star $ res)
          (agResult_cfg res)
          (agResult_blocks res)
          [agResult_init res]
          (AE S.empty)

      (context, effect) = maximalFixedPoint k monotoneFrameworkInstance
      pres = unlines . fmap presentAnalysis_AE . M.toList
  in report (fp ++ ": test_AExpr ")
      (unlines ["# Context:", pres context, "# Effect", pres effect])

-- Live variables

test_SLV :: Int -> FilePath -> Program -> String
test_SLV k fp p =
  let res = sem_Program' .  sem_Program $ p
      monotoneFrameworkInstance =
        mkMFInstance
          (SLV . S.fromList . agResult_all_vars $ res)
          (agResult_rcfg res)
          (agResult_blocks res)
          (agResult_finals res)
          (SLV S.empty)

      (context, effect) = maximalFixedPoint k monotoneFrameworkInstance
      pres = unlines . fmap presentAnalysis_SLV . M.toList
  in report (fp ++ ": test_StronglyLiveVariables")
      (unlines ["# Context:", pres context, "# Effect", pres effect])


-- Constant Propagation

test_CP :: Int -> FilePath -> Program -> String
test_CP k fp p =
  let res = sem_Program' . sem_Program $ p
      monotoneFrameworkInstance =
        let u = CP . M.fromList $ [(var, D Top) | var <- agResult_all_vars res]
        in mkMFInstance
            u -- Hetzelfde als top
            (agResult_cfg res)
            (agResult_blocks res)
            [agResult_init res]
            u -- \x -> Top voor elke variabele

      (context, effect) = maximalFixedPoint k monotoneFrameworkInstance
      pres = unlines . fmap presentAnalysis_CP . M.toList

  in report (fp ++ ": test_ConstantPropagation")
      (unlines ["# Context:", pres context, "# Effect", pres effect])


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

presentAnalysis_AE :: (Label, Analysis_AE) -> String
presentAnalysis_AE (l, (AE x)) =
  let
    aexprs = intercalate ", " . fmap show . S.toList $ x
  in show l ++ "\t{" ++ aexprs ++ "}"

presentAnalysis_SLV :: (Label, Analysis_SLV) -> String
presentAnalysis_SLV (l, (SLV x)) =
  let
    liveVars = intercalate ", " . S.toList $ x
  in show l ++ "\t{" ++ liveVars ++ "}"

presentAnalysis_CP :: (Label, Analysis_CP) -> String
presentAnalysis_CP (l, (CP x)) =
  let
    varmap =
      intercalate ", " . fmap (\(var, val) -> concat [var, "=", show val]) $ M.toList x
  in show l ++ "\t{" ++ varmap ++ "}"
