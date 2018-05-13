module Main.PhaseStaticChecks(phaseStaticChecks) where

import Main.CompileUtils
import Utils.Warnings(Warning)
import Utils.Messages
import qualified StaticAnalysis.StaticChecks as SC
import Syntax.UHA_Syntax (Name)
import Types (TpScheme)

import qualified Data.Set as S
import Data.Set (Set)

-- The ouput of this function tells you what the synthesized/chained
-- attributes we are in fact interested in. At this point, that is simply
-- an integer, counting the number of nodes in the ast.

phaseStaticChecks ::
   String -> Module -> [ImportEnvironment] -> [Option] ->
   Phase Error (Int, Int, [String], Set String, [(String,[(String,Int)])], [(String,Int)])
phaseStaticChecks fullName module_ importEnvs options = do
    enterNewPhase "Static checking" options

    let (_, baseName, _) = splitFilePath fullName

        -- Setting up two inherited attributes (to Module)
        -- by a suitable magic incantation
        res = SC.wrap_Module (SC.sem_Module module_) SC.Inh_Module {
            SC.importEnvironments_Inh_Module = importEnvs,
            SC.baseName_Inh_Module = baseName
        }

        size = SC.nrOfLeaves_Syn_Module res
        keywords = SC.reservedWords_Syn_Module res
        letDepth = SC.letDepth_Syn_Module res
        emptyClasses = SC.emptyClasses_Syn_Module res
        typeDecls = SC.typeDecls_Syn_Module res
        variables = SC.variables_Syn_Module res

    -- At this point, we just return the size. Not checking for errors yet,
    -- so we are always right.
    return (Right (size, letDepth, keywords, emptyClasses, typeDecls, variables))
