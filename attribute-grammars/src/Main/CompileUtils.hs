module Main.CompileUtils
    ( module Main.CompileUtils
    , Option(..)
    , splitFilePath, combinePathAndFile
    , when, unless
    , exitWith, ExitCode(..), exitSuccess, getArgs
    , module ModuleSystem.ImportEnvironment
    , Module(..)
    ) where

import Main.Args(Option(..))
import Utils.Messages(HasMessage)
import Control.Monad
import Utils.Utils(splitFilePath, combinePathAndFile)
import System.Exit
import System.Environment(getArgs)
import ModuleSystem.ImportEnvironment
import Syntax.UHA_Syntax(Module(..))
import Data.Maybe
import System.FilePath (joinPath)
import System.Process(system)

type Phase err a = IO (Either [err] a)
type CompileOptions = ([Option], String, [String]) 

(===>) :: Phase err1 a -> (a -> Phase err2 b) -> Phase (Either err1 err2) b
p ===> f = 
   p >>= either (return . Left . map Left) 
                (f >=> return . either (Left . map Right) Right)



doPhaseWithExit :: HasMessage err => Int -> ([err] -> String) -> CompileOptions -> Phase err a -> IO a
doPhaseWithExit nrOfMsgs code (_, _, _) phase =
   do result <- phase
      case result of
         Left errs ->
            do showErrorsAndExit errs nrOfMsgs
         Right a ->
            return a

sendLog :: String -> String -> [String] -> [Option] -> IO ()
sendLog _ _ _ _ = return ()
    --logger code (Just (modules,fullName))
    
enterNewPhase :: String -> [Option] -> IO ()
enterNewPhase phase options =
   when (Verbose `elem` options) $
      putStrLn (phase ++ "...")

showErrorsAndExit :: HasMessage a => [a] -> Int -> IO b
showErrorsAndExit errors maximumNumber = do
    let someErrors = take maximumNumber errors
    showMessages someErrors
    when (number > maximumNumber) $ 
        putStrLn "(...)\n"
    putStrLn ("Compilation failed with " ++ show number ++
                " error" ++ (if number == 1 then "" else "s"))
    exitWith (ExitFailure 1)
  where
    number = length errors

showMessages :: HasMessage a => [a] -> IO ()
showMessages =
    putStr . sortAndShowMessages  

sortAndShowMessages :: HasMessage a => [a] -> String
sortAndShowMessages = concatMap showMessage

showMessage :: HasMessage message => message -> String
showMessage _ = ""

    
sys :: String -> IO ()
sys s = do
    -- putStrLn ("System:" ++ s)
    _ <- system s
    return ()

