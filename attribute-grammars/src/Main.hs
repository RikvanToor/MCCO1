module Main where

import Parser.Parser(parseOnlyImports)
import Control.Monad
import System.FilePath(joinPath)
import Data.List(nub, elemIndex, isSuffixOf, isPrefixOf, intercalate)
import Data.Maybe(fromJust)
import System.Directory(doesFileExist, getModificationTime,
                        getPermissions, Permissions(writable))
import System.Exit
import Utils.Messages
import Utils.Utils
import Main.CompileUtils
import Main.Args
import Main.PhaseLexer
import Main.PhaseParser
import Main.PhaseStaticChecks
import Main.PhaseResolveOperators
import Parser.ParseMessage

import System.IO

import qualified Data.Set as S
import Data.Set (Set)

main :: IO ()
main = do
    args                     <- getArgs
    (options, Just fullName) <- processArgs args
    compile fullName
    return ()


compile :: String -> IO ()
compile fullName =
    do
        let options = []
        let doneModules = []
        let compileOptions = (options, fullName, doneModules)
        let importEnvs = [emptyEnvironment]
        putStrLn ("Lexing " ++ fullName)

        contents <- readSourceFile fullName

        -- Phase 1: Lexing
        (lexerWarnings, tokens) <-
            doPhaseWithExit 20 (const "L") compileOptions $
               phaseLexer fullName contents options

        unless (NoWarnings `elem` options) $
            showMessages lexerWarnings

        putStrLn ("Parsing " ++ fullName)

        -- Phase 2: Parsing
        parsedModule <-
            doPhaseWithExit 20 (const "P") compileOptions $
               phaseParser fullName tokens options

        putStrLn ("Resolving " ++ fullName)

        -- Phase 3: Resolving operators
        resolvedModule <-
            doPhaseWithExit 20 (const "R") compileOptions $
               phaseResolveOperators parsedModule importEnvs options

        putStrLn ("Checking " ++ fullName)

        -- NOTE: the next call is when everything you do should happen.
        -- The code that should be executed you should put in
        -- StaticAnalysis/
        -- Phase 4: Static checking

        (nrOfLeaves, letDepth, keywords, emptyClasses, typeDecls, variables, shadowing, samelevelshadowing) <-
            doPhaseWithExit 20 (const "S") compileOptions $
               phaseStaticChecks fullName resolvedModule importEnvs options


        putStrLn "Report:"
        putStrLn ("* Number of ast leaves: " ++ show nrOfLeaves)

        putStrLn ("* Maximum let depth: "    ++ show letDepth)
        putStrLn("* Used keywords: " ++ unwords keywords)

        mapM_ noInstanceDeclaredWarning (S.toList emptyClasses)

        printTypeDecls typeDecls

        -- Prints all variables
        -- hPutStrLn stderr("* Variables for debugging: " ++ show variables)
        --
        putStrLn (if shadowing then "* There is shadowing" else "There is no shadowing")
        printSameLevelShadowing samelevelshadowing

        putStrLn "Done now"

        --unless (NoWarnings `elem` options) $
        --    showMessages staticWarnings

printDeclaration :: (String, [(String, Int)]) -> IO ()
printDeclaration (n, ds) = do
                putStrLn $ "  * " ++ n ++ " is declared at:"
                let q :: (String, Int) -> IO ()
                    q (f, l) = do
                    putStrLn $ "    - " ++ f ++ " on line " ++ (show l)
                mapM_ q ds

printTypeDecls :: [(String, [(String, Int)])] -> IO ()
printTypeDecls [] = putStrLn ("* No double type declarations")
printTypeDecls xs = do
    putStrLn "* Error: Types are declared multiple times:"
    mapM_ printDeclaration xs

printSameLevelShadowing :: [(String, [(String, Int)])] -> IO ()
printSameLevelShadowing [] = putStrLn ("* No same level shadowing")
printSameLevelShadowing xs = do
    putStrLn "* Error: Same level shadowing:"
    mapM_ printDeclaration xs

stopCompilingIf :: Bool -> IO ()
stopCompilingIf bool = when bool (exitWith (ExitFailure 1))

noInstanceDeclaredWarning :: String -> IO ()
noInstanceDeclaredWarning str =
  warning ("No instance declared for class " ++ str)

warning :: String -> IO ()
warning str = putStrLn ("Warning: " ++ str)
