module Main.Compile where

import Main.PhaseLexer
import Main.PhaseParser
import Main.PhaseResolveOperators
import Main.PhaseStaticChecks
import Main.CompileUtils
import Utils.Utils
import Data.IORef

compile :: String -> IO ()
compile fullName =
    do
        let options = []
        let doneModules = []
        let compileOptions = (options, fullName, doneModules)
        putStrLn ("Doing " ++ fullName)

        contents <- readSourceFile fullName

        -- Phase 1: Lexing
        (lexerWarnings, tokens) <- 
            doPhaseWithExit 20 (const "L") compileOptions $
               phaseLexer fullName contents options
        
        unless (NoWarnings `elem` options) $
            showMessages lexerWarnings

        -- Phase 2: Parsing
        parsedModule <- 
            doPhaseWithExit 20 (const "P") compileOptions $
               phaseParser fullName tokens options

        -- Phase 3: Resolving operators
        resolvedModule <- 
            doPhaseWithExit 20 (const "R") compileOptions $
               phaseResolveOperators parsedModule importEnvs options
                           
        -- Phase 4: Static checking
        (localEnv, typeSignatures, staticWarnings) <-
            doPhaseWithExit 20 (cons "S") compileOptions $
               phaseStaticChecks fullName resolvedModule importEnvs options        

        unless (NoWarnings `elem` options) $
            showMessages staticWarnings


stopCompilingIf :: Bool -> IO ()
stopCompilingIf bool = when bool (exitWith (ExitFailure 1))

