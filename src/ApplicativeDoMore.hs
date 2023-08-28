
module ApplicativeDoMore where

import Data.Generics qualified as SYB
import GHC.Hs
import GHC.Plugins qualified as GHC
import GHC.Rename.Expr qualified as Rename
import GHC.Tc.Types qualified as GHC
import GHC.Tc.Utils.Monad qualified as GHC
import GHC.Types.Basic
import GHC.Types.Name.Set
import GHC.Types.SrcLoc
import Language.Haskell.TH qualified as TH


-- * Haskell Plugin
---------------------------------------------------------------------

plugin :: GHC.Plugin
plugin = GHC.defaultPlugin
    { GHC.renamedResultAction
        = afterRename
    , GHC.pluginRecompile
        = \ _cli -> pure GHC.NoForceRecompile
    }

-- TODOs:
-- 
--  - Rename statements to get free variable sets.
--  
--  - Dependency analysis.
--  
--  - 
    
afterRename
    :: [GHC.CommandLineOption]
    -> GHC.TcGblEnv
    -> HsGroup GhcRn
    -> GHC.TcM (GHC.TcGblEnv, HsGroup GhcRn)

afterRename _cli env hsgroup =
    (env,) <$> applicativeDoRewrite hsgroup


-- * Applicative Do Rewrite Rule
---------------------------------------------------------------------

applicativeDoRewrite
    :: HsGroup GhcRn
    -> GHC.TcM (HsGroup GhcRn)

applicativeDoRewrite = SYB.everywhereM (SYB.mkM rewrite)
    where
    rewrite :: LHsExpr GhcPs -> GHC.TcM (LHsExpr GhcPs)
    
    rewrite e@(L loc (HsDo _ _flavour (L _ stmts))) = do
        GHC.printForUserTcRn $ GHC.ppr stmts
        pure e
        
    rewrite e = pure e

