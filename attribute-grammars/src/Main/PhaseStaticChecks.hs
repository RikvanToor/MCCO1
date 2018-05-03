module Main.PhaseStaticChecks(phaseStaticChecks) where

import Main.CompileUtils
import Utils.Warnings(Warning)
import Utils.Messages
import qualified StaticAnalysis.StaticChecks as SC
import Syntax.UHA_Syntax (Name)
import Types (TpScheme)

-- The ouput of this function tells you what the synthesized/chained
-- attributes we are in fact interested in. At this point, that is simply
-- an integer, counting the number of nodes in the ast.
phaseStaticChecks ::
   String -> Module -> [ImportEnvironment] -> [Option] ->
   Phase Error (Int, Int)
phaseStaticChecks fullName module_ importEnvs options = do
    enterNewPhase "Static checking" options

    let (_, baseName, _) = splitFilePath fullName

        -- Setting up two inherited attributes (to Module)
        -- by a suitable magic incantation
        res = SC.wrap_Module (SC.sem_Module module_) SC.Inh_Module {
            SC.importEnvironments_Inh_Module = importEnvs,
            SC.baseName_Inh_Module = baseName
        }

    -- At this point, we just return the size. Not checking for errors yet,
    -- so we are always right.
    return (Right (SC.nrOfLeaves_Syn_Module res, SC.letDepth_Syn_Module res))