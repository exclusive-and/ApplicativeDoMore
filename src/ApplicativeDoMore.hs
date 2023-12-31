
module ApplicativeDoMore where

import Data.Generics qualified as SYB
import Data.Maybe (isNothing)
import GHC.Builtin.Names
import GHC.Hs
import GHC.Plugins qualified as GHC
import GHC.Rename.Env
import GHC.Tc.Types (TcM, TcGblEnv)
import GHC.Tc.Utils.Monad (traceRn, wrapLocMA)
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.SrcLoc


-- * Haskell Plugin
---------------------------------------------------------------------

plugin :: GHC.Plugin

plugin = GHC.defaultPlugin
    { GHC.renamedResultAction = afterRename
    , GHC.pluginRecompile     = GHC.purePlugin
    }
    where
    afterRename
        :: [GHC.CommandLineOption]
        -> TcGblEnv
        -> HsGroup GhcRn
        -> TcM (TcGblEnv, HsGroup GhcRn)

    afterRename _cli env hsgroup =
        (env,) <$> applicativeDoRewrite hsgroup


-- * Applicative Do Rewrite Rule
---------------------------------------------------------------------

-- |
-- Rewrite do-expressions so that let-statements only need 'Applicative'.
-- 
-- GHC's renamer does the heavy lifting. All that's left to do is:
-- 
--  - Collect all the 'LetStmt' binders.
--
--  - Bring the collected binders inside the 'LastStmt'.
--
--  - Remove 'join' calls from any 'ApplicativeStmt's.
-- 
-- If any one of these steps fails, we fall back on the original expression
-- and let TC incur the 'Monad' constraint.
--   
applicativeDoRewrite
    :: HsGroup GhcRn
    -> TcM (HsGroup GhcRn)

applicativeDoRewrite = SYB.everywhereM (SYB.mkM rewriteLExpr)
    where
    rewriteLExpr :: LHsExpr GhcRn -> TcM (LHsExpr GhcRn)
    rewriteLExpr = wrapLocMA rewrite
    
    rewrite :: HsExpr GhcRn -> TcM (HsExpr GhcRn)
    
    rewrite (HsDo _ flavour (L loc stmts0)) = do
        stmts1 <- rewriteDoStmts flavour stmts0
        stmts2 <- postprocessApplicativeStmts flavour stmts1
        traceRn "applicativeDoRewrite-result" $ GHC.ppr stmts2
        pure (HsDo noExtField flavour (L loc stmts2))
        
    rewrite e = pure e


-- * Do-Expression Rearrangement
---------------------------------------------------------------------

-- |
-- Try to collect let-binders and move them to the last statement.
-- 
-- Skips rewriting if 'rearrangeLetStmts' fails.
-- 
rewriteDoStmts
    :: HsDoFlavour
    -> [ExprLStmt GhcRn]
    -> TcM [ExprLStmt GhcRn]
    
rewriteDoStmts flavour = go . reverse
    -- Note that we process statements in reverse order. This allows
    -- 'rearrangeLetStmts' to move let-statements that bind variables in
    -- other let-statements. See [Variables bindings in moveable expressions].
    where
    go (stmt@(L loc (LastStmt _ body stripped ret)) : stmts) = do
        re <- rearrangeLetStmts stmts
        case re of
            Just (binds, same) -> do
                last' <- rewriteLast flavour binds body stripped ret
                pure (same ++ [L loc last'])
            Nothing -> pure (reverse (stmt : stmts))
        
    go _ = error "No last statement"

-- |
-- Try to collect the let-binders.
-- 
-- Fails if the main body of the do-expression is anything but 'LetStmt's or
-- 'ApplicativeStmt's. Any other statements irrevocably invoke monadic binds.
-- 
rearrangeLetStmts
    :: [ExprLStmt GhcRn]
    -> TcM (Maybe (ValBinds, [ExprLStmt GhcRn]))
    
rearrangeLetStmts = go emptyValBinds []
    where
    go
        :: ValBinds                  -- Moveable bindings
        -> [ExprLStmt GhcRn]         -- Statements to keep the same
        -> [ExprLStmt GhcRn]         -- Statements left to process
        -> TcM (Maybe (ValBinds, [ExprLStmt GhcRn]))
    
    go move same [] = pure (Just (move, same))
    
    -- Check whether a 'LetStmt' binds variables in later statements.
    -- 
    -- If they don't bind variables in anything but the 'LastStmt', then
    -- it's safe to move them.
    -- 
    -- If they do bind variables in other statements, leave them as-is.
    -- These will incur a 'Monad' constraint, but there's nothing we can do.
    -- 
    -- [Variable bindings in moveable expressions]
    -- Note that we don't check the expressions we already plan to move.
    -- We view this statement binding a variable in one of those expressions
    -- as if it were binding a variable in the 'LastStmt'.
    go move same (stmt@(L _ (LetStmt _ binds)) : stmts) = do
        let bindNames = mkNameSet $ collectBinders binds
        binds' <- collectBinds binds
        if bindNames `bindsIn` same
            then go move (stmt : same) stmts
            else go (binds' `appendBinds` move) same stmts
    
    -- Leave 'ApplicativeStmt's as-is.
    -- 
    -- TODO: Recursively find moveable 'LetStmt's inside 'ApplicativeStmt's.
    go move same (stmt@(L _ ApplicativeStmt{}) : stmts) =
        go move (stmt : same) stmts
    
    -- [Single binding in do-expression]
    -- There is an edge case where a do-expression consists of exactly one
    -- bind, followed by a 'LetStmt'. In that case, the renamer won't
    -- rewrite the bind as an 'ApplicativeStmt', even though it is still
    -- considered valid applicative do notation.
    go move same (stmt@(L _ BindStmt{}) : []) =
        pure (Just (move, stmt : same))
    
    -- If we get something other than a 'LetStmt' or an 'ApplicativeStmt',
    -- then we'll incur 'Monad' anyway. Fail out immediately.
    go _move _same _ = pure Nothing
    
-- |
-- Check whether a set of names would bind variables in subsequent statements.
-- 
bindsIn :: NameSet -> [ExprLStmt GhcRn] -> Bool
bindsIn binds = go
    where
    go [] = False
    
    go (stmt : stmts) = not (null foundbinds) || go stmts
        where foundbinds = SYB.listify (`elemNameSet` binds) stmt


mkHsLet :: HsLocalBinds GhcRn -> LHsExpr GhcRn -> HsExpr GhcRn
mkHsLet binds expr = HsLet noExtField noHsTok binds noHsTok expr

-- |
-- Rewrite the last statement of a do-expression. Plugs the original body
-- expression of the statement into a let-expression.
-- 
rewriteLast
    :: HsDoFlavour
    -> ValBinds
    -> LHsExpr GhcRn
    -> Maybe Bool
    -> SyntaxExpr GhcRn
    -> TcM (ExprStmt GhcRn)
    
rewriteLast ctx binds body _stripped ret = do
    (returnName, _) <- lookupQualifiedDoName (HsDoStmt ctx) returnMName
    (pureName  , _) <- lookupQualifiedDoName (HsDoStmt ctx) pureAName
    
    let binds' :: HsLocalBinds GhcRn
        binds' = HsValBinds noAnn $ XValBindsLR binds
    
    let body' = rewriteReturn returnName pureName (noLocA . mkHsLet binds')
                              body
    
    pure (LastStmt noExtField body' (Just True) ret)

-- |
-- Rewrite the return expression at the end of a do-expression.
-- 
-- Strips occurrences of 'return' or 'pure'.
-- 
rewriteReturn
    :: Name
    -> Name
    -> (LHsExpr GhcRn -> LHsExpr GhcRn)
    -> LHsExpr GhcRn
    -> LHsExpr GhcRn
    
rewriteReturn returnName pureName rewriteLet (L loc e) =
    case e of
        -- return $ or pure $
        OpApp _x l op r
            | isReturn l, isDollar op
            -> rewriteLet r
        -- return or pure, without $
        HsApp _x f arg
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

-- |
-- Try to remove any occurances of 'join' from the 'ApplicativeStmt's.
-- 
-- If we find that we can't remove 'join' from one of the statements, fall
-- back on the original expression with 'join's intact.
-- 
postprocessApplicativeStmts
    :: HsDoFlavour
    -> [ExprLStmt GhcRn]
    -> TcM [ExprLStmt GhcRn]

postprocessApplicativeStmts flavour lstmts = go [] $ reverse lstmts
    where
    go
        :: [ExprLStmt GhcRn]
        -> [ExprLStmt GhcRn]
        -> TcM [ExprLStmt GhcRn]
    
    go acc [] = pure acc
    
    -- Always keep the last statement.
    go acc (stmt@(L _ (LastStmt{})) : stmts) =
        go (stmt : acc) stmts
    
    -- As long as this do-expression is only made of 'ApplicativeStmt's and
    -- a single 'LastStmt', we can rewrite it without 'join'.
    go acc (L loc (ApplicativeStmt x args _) : stmts) =
        go (L loc (ApplicativeStmt x args Nothing) : acc) stmts
    
    -- Got a single 'BindStmt', so we need to rewrite it as an
    -- 'ApplicativeStmt'.
    -- 
    -- See note [Single binding in do-expression].
    go acc (L loc (BindStmt _ pat expr) : []) = do
        (fmapOp, _) <-
            lookupQualifiedDoStmtName (HsDoStmt flavour) fmapName
        let arg  = ApplicativeArgOne Nothing pat expr False
            args = [(fmapOp, arg)]
        pure (L loc (ApplicativeStmt noExtField args Nothing) : acc)
    
    -- Anything else and we immediately fail out.
    go _acc (_ : _stmts) = pure lstmts

-- ** GHC.Rename.Expr Artefacts
---------------------------------------------------------------------

lookupQualifiedDoStmtName
    :: HsStmtContext GhcRn
    -> Name
    -> TcM (SyntaxExpr GhcRn, FreeVars)

lookupQualifiedDoStmtName ctx name =
    case qualifiedDoModuleName_maybe ctx of
        Nothing -> lookupStmtName ctx name
        Just modName -> do
            (name', fvs) <- lookupNameWithQualifier name modName
            pure (mkSyntaxExpr $ nl_HsVar name', fvs)

lookupStmtName
    :: HsStmtContext GhcRn
    -> Name
    -> TcM (SyntaxExpr GhcRn, FreeVars)

lookupStmtName ctx name
    | isRebindableContext ctx
    = lookupSyntax name
    | otherwise
    = pure (mkRnSyntaxExpr name, emptyFVs)

isRebindableContext :: HsStmtContext GhcRn -> Bool
isRebindableContext = \case
    HsDoStmt flavour
        | ListComp <- flavour       -> False
        | DoExpr m <- flavour       -> isNothing m
        | MDoExpr m <- flavour      -> isNothing m
        | MonadComp <- flavour      -> True
        | GhciStmtCtxt <- flavour   -> True
    
    ArrowExpr           -> False
    PatGuard{}          -> False
    ParStmtCtxt ctx     -> isRebindableContext ctx
    TransStmtCtxt ctx   -> isRebindableContext ctx


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

collectBinds :: HsLocalBinds GhcRn -> TcM ValBinds
collectBinds = \case
    HsValBinds _ (XValBindsLR binds)
        -> pure binds
    bs  -> GHC.pprPanic "collectBinds" (GHC.ppr bs)

appendBinds :: ValBinds -> ValBinds -> ValBinds
appendBinds (NValBinds xs xsigs) (NValBinds ys ysigs) =
    NValBinds (xs ++ ys) (xsigs ++ ysigs)

collectBinders :: HsLocalBinds GhcRn -> [IdP GhcRn]
collectBinders = collectLocalBinders CollNoDictBinders

