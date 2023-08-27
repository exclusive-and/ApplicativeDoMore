
module ApplicativeDoMore where

import Data.Generics qualified as SYB
import GHC.Hs
import GHC.Plugins qualified as GHC
import GHC.Tc.Types qualified as GHC
import GHC.Types.Basic
import GHC.Types.Name.Set
import GHC.Types.SrcLoc
import Language.Haskell.TH qualified as TH


-- * Haskell Plugin
---------------------------------------------------------------------

plugin :: GHC.Plugin
plugin = GHC.defaultPlugin
    { GHC.renamedResultAction
        = \ _cli -> applicativeDoMorePlugin _cli
    , GHC.pluginRecompile
        = \ _cli -> pure GHC.NoForceRecompile
    }

applicativeDoMorePlugin
    :: [GHC.CommandLineOption]
    -> GHC.TcGblEnv
    -> HsGroup GhcRn
    -> GHC.TcM (GHC.TcGblEnv, HsGroup GhcRn)

applicativeDoMorePlugin _cli env hsgroup =
    (env,) <$> applicativeDoRewrite hsgroup


-- * Applicative Do Rewrite Rule
---------------------------------------------------------------------

applicativeDoRewrite
    :: HsGroup GhcRn
    -> GHC.TcM (HsGroup GhcRn)

applicativeDoRewrite = SYB.everywhereM (SYB.mkM rewrite)
    where
    rewrite :: LHsExpr GhcPs -> GHC.TcM (LHsExpr GhcPs)
    rewrite (L loc (HsDo _ _ctx (L _ stmts))) = do
        undefined
    rewrite e = pure e


data ApplicativeTree' a
    = One   a
    | Bind  (ApplicativeTree' a) (ApplicativeTree' a)
    | App   [ApplicativeTree' a]

type ApplicativeStmt = (ExprLStmt GhcPs, FreeVars)
type ApplicativeTree = ApplicativeTree' ApplicativeStmt

    
applicativeTree :: [ApplicativeStmt] -> ApplicativeTree
applicativeTree [one] = One one
applicativeTree stmts =
    case segments stmts of
        [one] -> split one
        segs  -> App (map split segs)
    where
        split [one] = One one
        split stmts = Bind (applicativeTree before) (applicativeTree after)
            where (before, after) = separate stmts

segments :: [ApplicativeStmt] -> [[ApplicativeStmt]]
segments = undefined

separate
    :: [ApplicativeStmt]
    -> ([ApplicativeStmt], [ApplicativeStmt])

separate = undefined
