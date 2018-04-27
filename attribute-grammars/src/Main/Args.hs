module Main.Args
    ( Option(..), processArgs) where

import System.Exit
import Main.Version
import Control.Monad(when)
import System.Console.GetOpt

simplifyOptions :: a -> a
simplifyOptions = id

terminateWithMessage :: [Option] -> String -> [String] -> IO ([Option], Maybe String)
terminateWithMessage _ message errors = do
    putStrLn message
    putStrLn (unlines errors)
    putStrLn $ "Your First Compiler " ++ version
    putStrLn "Usage: cco-ag file"
    exitWith (ExitFailure 1)

processArgs :: [String] -> IO ([Option], Maybe String)
processArgs args = do
    (options, maybeFiles) <- basicProcessArgs [DisableLogging, Overloading] args
    case maybeFiles of
        Nothing ->
          terminateWithMessage options "Error in invocation: the name of the module to be compiled seems to be missing." []
        Just _ ->
          return ([], maybeFiles)
          
-- The Maybe String indicates that a file may be missing. Resulting options are simplified
basicProcessArgs :: [Option] -> [String] ->  IO ([Option], Maybe String)
basicProcessArgs defaults args =
    let (options, arguments, errors) = getOpt Permute (optionDescription True True) args
    in if not (null errors) then
          terminateWithMessage options "Error in invocation: list of parameters is erroneous.\nProblem(s):"
                               (map ("  " ++) errors)
    else        
        if length arguments > 1 then
            terminateWithMessage options ("Error in invocation: only one non-option parameter expected, but found instead:\n" ++ unlines (map ("  "++) arguments)) []
        else
            do
              let simpleOptions = simplifyOptions (defaults ++ options)
                  argument = if null arguments then Nothing else Just (head arguments)
              when (Verbose `elem` simpleOptions) $ do
                mapM_ putStrLn ("Options after simplification: " : map show simpleOptions)
                putStrLn ("Argument: " ++ show argument)
              return (simpleOptions, argument)

optionDescription :: Bool -> Bool -> [OptDescr Option]
optionDescription _ _ = []

data Option
   -- Main options
   = BuildOne | BuildAll | DumpInformationForThisModule | DumpInformationForAllModules
   | DisableLogging | EnableLogging | Alert String | Overloading | NoOverloading | LvmPath String | Verbose | NoWarnings | MoreOptions
   | Information String | BasePath String
   -- More options
   | StopAfterParser | StopAfterStaticAnalysis | StopAfterTypeInferencing | StopAfterDesugar
   | DumpTokens | DumpUHA | DumpCore | DumpCoreToFile
   | DebugLogger | Host String | Port Int
   | DumpTypeDebug | AlgorithmW | AlgorithmM | DisableDirectives | NoRepairHeuristics | HFullQualification
   -- Experimental options
   | ExperimentalOptions | KindInferencing | SignatureWarnings | RightToLeft | NoSpreading
   | TreeWalkTopDown | TreeWalkBottomUp | TreeWalkInorderTopFirstPre | TreeWalkInorderTopLastPre
   | TreeWalkInorderTopFirstPost | TreeWalkInorderTopLastPost | SolverSimple | SolverGreedy
   | SolverTypeGraph | SolverCombination | SolverChunks | UnifierHeuristics
   | SelectConstraintNumber Int | NoOverloadingTypeCheck | NoPrelude | UseTutor
   | RepairDepth Int | RepairEvalLimit Int -- Arjen Langebaerd's work 
 deriving (Eq)


instance Show Option where
 show BuildOne                           = "--build"
 show BuildAll                           = "--build-all"
 show DumpInformationForThisModule       = "--dump-information"
 show DumpInformationForAllModules       = "--dump-all-information"
 show EnableLogging                      = "--enable-logging"
 show DisableLogging                     = "--disable-logging"
 show (Alert str)                        = "--alert=\"" ++ str ++ "\"" -- May contain spaces
 show Overloading                        = "--overloading"
 show NoOverloading                      = "--no-overloading"
 show (LvmPath str)                      = "--lvmpath=\"" ++ str ++ "\"" -- May contain spaces
 show (BasePath str)                      = "--basepath=\"" ++ str ++ "\"" -- May contain spaces
 show Verbose                            = "--verbose"
 show NoWarnings                         = "--no-warnings"
 show MoreOptions                        = "--moreoptions"
 show (Information str)                  = "--info=" ++ str
 show StopAfterParser                    = "--stop-after-parsing"
 show StopAfterStaticAnalysis            = "--stop-after-static-analysis"
 show StopAfterTypeInferencing           = "--stop-after-type-inferencing"
 show StopAfterDesugar                   = "--stop-after-desugaring"
 show DumpTokens                         = "--dump-tokens"
 show DumpUHA                            = "--dump-uha"
 show DumpCore                           = "--dump-core"
 show DumpCoreToFile                     = "--save-core"
 show DebugLogger                        = "--debug-logger"
 show (Host host)                        = "--host=" ++ host
 show (Port port)                        = "--port=" ++ show port
 show DumpTypeDebug                      = "--type-debug"
 show AlgorithmW                         = "--algorithm-w"
 show AlgorithmM                         = "--algorithm-m"
 show DisableDirectives                  = "--no-directives"
 show NoRepairHeuristics                 = "--no-repair-heuristics"
 show ExperimentalOptions                = "--experimental-options"
 show KindInferencing                    = "--kind-inferencing"
 show SignatureWarnings                  = "--signature-warnings"
 show RightToLeft                        = "--right-to-left"
 show NoSpreading                        = "--no-spreading"
 show TreeWalkTopDown                    = "--treewalk-topdown"
 show TreeWalkBottomUp                   = "--treewalk-bottomup"
 show TreeWalkInorderTopFirstPre         = "--treewalk-inorder1"
 show TreeWalkInorderTopLastPre          = "--treewalk-inorder2"
 show TreeWalkInorderTopFirstPost        = "--treewalk-inorder3"
 show TreeWalkInorderTopLastPost         = "--treewalk-inorder4"
 show SolverSimple                       = "--solver-simple"
 show SolverGreedy                       = "--solver-greedy"
 show SolverTypeGraph                    = "--solver-typegraph"
 show SolverCombination                  = "--solver-combination"
 show SolverChunks                       = "--solver-chunks"
 show UnifierHeuristics                  = "--unifier-heuristics"
 show (SelectConstraintNumber cnr)       = "--select-cnr=" ++ show cnr
 show HFullQualification                 = "--H-fullqualification"
 show NoOverloadingTypeCheck             = "--no-overloading-typecheck"
 show NoPrelude                          = "--no-prelude"
 show UseTutor                           = "--use-tutor"
 show (RepairDepth depth)                = "--repair-depth=" ++ show depth
 show (RepairEvalLimit limit)            = "--repair-eval-limit=" ++ show limit

