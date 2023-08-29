
module ApplicativeDoMore where

import Data.Generics qualified as SYB
import GHC.Builtin.Names
import GHC.Hs
import GHC.Plugins qualified as GHC
import GHC.Rename.Env (lookupQualifiedDoName)
import GHC.Tc.Types qualified as GHC
import GHC.Tc.Utils.Monad qualified as GHC
import GHC.Types.Name.Set
import GHC.Types.SrcLoc


-- * Haskell Plugin
---------------------------------------------------------------------

plugin :: GHC.Plugin
plugin = GHC.defaultPlugin
    { GHC.renamedResultAction
        = afterRename
    , GHC.pluginRecompile
        = \ _cli -> pure GHC.NoForceRecompile
    }
    
afterRename
    :: [GHC.CommandLineOption]
    -> GHC.TcGblEnv
    -> HsGroup GhcRn
    -> GHC.TcM (GHC.TcGblEnv, HsGroup GhcRn)

afterRename _cli env hsgroup = do
    (env,) <$> applicativeDoRewrite hsgroup


-- * Applicative Do Rewrite Rule
---------------------------------------------------------------------

applicativeDoRewrite
    :: HsGroup GhcRn
    -> GHC.TcM (HsGroup GhcRn)

applicativeDoRewrite = SYB.everywhereM (SYB.mkM rewriteLExpr)
    where
    rewriteLExpr :: LHsExpr GhcRn -> GHC.TcM (LHsExpr GhcRn)
    rewriteLExpr = GHC.wrapLocMA rewrite
    
    rewrite :: HsExpr GhcRn -> GHC.TcM (HsExpr GhcRn)
    
    rewrite (HsDo _ flavour (L loc stmts0)) = do
        GHC.traceRn "applicativeDoRewrite" $ GHC.ppr stmts0
        stmts1 <- rearrangeLStmts flavour stmts0
        stmts2 <- postprocessLStmts stmts1
        GHC.traceRn "applicativeDoRewrite" $ GHC.ppr stmts2
        pure (HsDo noExtField flavour (L loc stmts2))
        
    rewrite e = pure e

-- * Do-Expression Rearrangement
---------------------------------------------------------------------
    
rearrangeLStmts
    :: HsDoFlavour
    -> [ExprLStmt GhcRn]
    -> GHC.TcM [ExprLStmt GhcRn]
    
rearrangeLStmts flavour = go emptyValBinds []
    where
    go
        :: ValBinds                  -- Bindings we've moved
        -> [ExprLStmt GhcRn]         -- Statements we've kept the same
        -> [ExprLStmt GhcRn]         -- Statements left to process
        -> GHC.TcM [ExprLStmt GhcRn]
    
    -- When we get to the last statement, try rewriting it.
    go moved same [L loc (LastStmt _ body stripped ret)] = do
        stmt' <- rewriteLast flavour moved body stripped ret
        pure $ reverse (L loc stmt' : same)
    
    -- Try to move let-bindings into the last statement.
    go moved same (stmt@(L _ (LetStmt _ binds)) : stmts) = do
        let bindNames = mkNameSet $ collectBinders binds
        binds' <- collectBinds binds
        if bindsIn bindNames stmts
            then go moved (stmt : same) stmts
            else go (binds' `appendBinds` moved) same stmts
    
    -- Keep anything that can't be moved exactly as-is.
    go moved same (stmt : stmts) = do
        GHC.traceRn "bringLetsToEnd" $ GHC.ppr stmts
        go moved (stmt : same) stmts
    
    go _ _ [] = error "No valid last statement"

-- |
-- Check whether a set of names would bind variables in subsequent statements.
-- 
bindsIn :: NameSet -> [ExprLStmt GhcRn] -> Bool
bindsIn binds = go
    where
    go []     = False
    go [_one] = False
    go (stmt : stmts) = not (null foundbinds) || go stmts
        where foundbinds = SYB.listify (`elemNameSet` binds) stmt


mkHsLet :: HsLocalBinds GhcRn -> LHsExpr GhcRn -> HsExpr GhcRn
mkHsLet binds expr = HsLet noExtField letTok binds inTok expr
    where
    letTok = L NoTokenLoc HsTok :: LHsToken "let" GhcRn
    inTok  = L NoTokenLoc HsTok :: LHsToken "in" GhcRn

-- |
-- Try to rewrite the last statement in a do-expression as a let-block with
-- the provided bindings.
-- 
rewriteLast
    :: HsDoFlavour
    -> ValBinds
    -> LHsExpr GhcRn
    -> Maybe Bool
    -> SyntaxExpr GhcRn
    -> GHC.TcM (ExprStmt GhcRn)
    
rewriteLast ctx binds body stripped ret = do
    (returnName, _) <- lookupQualifiedDoName (HsDoStmt ctx) returnMName
    (pureName  , _) <- lookupQualifiedDoName (HsDoStmt ctx) pureAName
    
    let binds' :: HsLocalBinds GhcRn
        binds' = HsValBinds noAnn $ XValBindsLR binds
    
    let body' = rewriteReturn returnName pureName (noLocA . mkHsLet binds') body
    
    pure (LastStmt noExtField body' (Just True) ret)

rewriteReturn
    :: GHC.Name
    -> GHC.Name
    -> (LHsExpr GhcRn -> LHsExpr GhcRn)
    -> LHsExpr GhcRn
    -> LHsExpr GhcRn
    
rewriteReturn returnName pureName rewriteLet (L loc e) =
    case e of
        -- return $ or pure $
        OpApp x l op r
            | isReturn l, isDollar op
            -> rewriteLet r
        -- return or pure, without $
        HsApp x f arg
            | isReturn f
            -> rewriteLet arg
        _
            -> rewriteLet (L loc e)
    where
        isVar f (L _ (HsPar _ _ x _)) = isVar f x
        isVar f (L _ (HsAppType _ x _ _)) = isVar f x
        isVar f (L _ (HsVar _ (L _ name))) = f name
        
        isVar _ _ = False
        
        isReturn = isVar (\n -> n == returnName || n == pureName)
        isDollar = isVar (`hasKey` dollarIdKey)


-- * ApplicativeStmt Post-Processing
---------------------------------------------------------------------

postprocessLStmts :: [ExprLStmt GhcRn] -> GHC.TcM [ExprLStmt GhcRn]
postprocessLStmts lstmts = go [] $ reverse lstmts
    where
    go :: [ExprLStmt GhcRn] -> [ExprLStmt GhcRn] -> GHC.TcM [ExprLStmt GhcRn]
    
    go acc [] = pure acc
    
    -- Always keep the last statement.
    go acc (stmt@(L _ (LastStmt{})) : stmts) =
        go (stmt : acc) stmts
    
    -- As long as this do-expression is only made of 'ApplicativeStmt's and
    -- a single 'LastStmt', we can rewrite it without 'join'.
    go acc (L loc (ApplicativeStmt x args _) : stmts) =
        go (L loc (ApplicativeStmt x args Nothing) : acc) stmts
    
    -- Anything else and we immediately fail out.
    go _acc (_ : _stmts) = pure lstmts


-- * Variable Bindings
---------------------------------------------------------------------

-- |
-- Since the plugin runs after renaming, all our value bindings will be
-- in the form of 'NHsValBindsLR'.
-- 
-- See <https://hackage.haskell.org/package/ghc-9.6.1/docs/src/GHC.Rename.Bind.html#rnValBindsRHS>
-- 
type ValBinds = NHsValBindsLR GhcRn

emptyValBinds :: ValBinds
emptyValBinds = NValBinds [] []

collectBinds :: HsLocalBinds GhcRn -> GHC.TcM ValBinds
collectBinds = \case
    HsValBinds _ (XValBindsLR binds)
        -> pure binds
    bs  -> GHC.pprPanic "collectBinds" (GHC.ppr bs)

appendBinds :: ValBinds -> ValBinds -> ValBinds
appendBinds (NValBinds xs xsigs) (NValBinds ys ysigs) =
    NValBinds (xs ++ ys) (xsigs ++ ysigs)

collectBinders :: HsLocalBinds GhcRn -> [IdP GhcRn]
collectBinders = collectLocalBinders CollNoDictBinders

    
