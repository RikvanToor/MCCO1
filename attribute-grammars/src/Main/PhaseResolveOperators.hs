module Main.PhaseResolveOperators(phaseResolveOperators) where

import Main.CompileUtils
import Parser.ResolveOperators(resolveOperators, operatorsFromModule, ResolveError)
import qualified Syntax.UHA_Pretty as PP(sem_Module,wrap_Module,Inh_Module(..),text_Syn_Module)
import qualified Data.Map as M

phaseResolveOperators :: 
   Module -> [ImportEnvironment] -> [Option] -> 
   Phase ResolveError Module

phaseResolveOperators moduleBeforeResolve importEnvs options = do
    enterNewPhase "Resolving operators" options

    let importOperatorTable = 
            M.unions (operatorsFromModule moduleBeforeResolve : map operatorTable importEnvs)
                          
        (module_, resolveErrors) = 
                  resolveOperators importOperatorTable moduleBeforeResolve

    case resolveErrors of
       
       _:_ ->
          return (Left resolveErrors)
          
       [] ->
          do when (DumpUHA `elem` options) $
                print $ PP.text_Syn_Module $ PP.wrap_Module (PP.sem_Module module_) PP.Inh_Module
    
             return (Right module_)

    

