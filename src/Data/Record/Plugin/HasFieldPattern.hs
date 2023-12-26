{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Record.Plugin.HasFieldPattern (plugin) where

import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Monoid
import Data.Bifunctor (first)
import Control.Monad.Trans.Writer.CPS
import Data.Record.Plugin.Shim

-- for check required extensions
import Control.Monad.Except
import Language.Haskell.TH (Extension(..))
import Data.List (intersperse)


{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

plugin :: Plugin
plugin = defaultPlugin {
      parsedResultAction = aux
    , pluginRecompile    = purePlugin
    }
  where
    aux ::
         [CommandLineOption]
      -> ModSummary
      -> HsParsedModule -> Hsc HsParsedModule
    aux _opts _summary parsed@HsParsedModule{
      hpm_module =L l modl@HsModule{
                     hsmodDecls   = decls
                   , hsmodImports = imports
                   } } = do
      checkEnabledExtensions l                
      let (decls', needImports) = runWriter $ transformBiM transformPat decls
      let modls = if getAny needImports then [importDecl ghcRecordsCompat True] else []
      return $ parsed {
        hpm_module = L l $ modl {
            hsmodDecls   = decls'
          , hsmodImports = imports ++ modls
          }
        }

{-------------------------------------------------------------------------------
  Main translation
-------------------------------------------------------------------------------}

transformPat :: LPat GhcPs -> Writer Any (LPat GhcPs)
transformPat p
  | Just (L (SrcSpanAnn _ l) nm, RecCon (HsRecFields flds dotdot)) <- viewConPat p
  , Unqual nm' <- nm
  , Nothing    <- dotdot
  , Just flds' <- mapM getFieldSel flds
  , parseRec (occNameString nm')
  =  mkRecPat l flds'

  | otherwise
  = return p

parseRec :: String -> Bool
parseRec "REC" = True
parseRec _ = False

mkRecPat ::
     SrcSpan
  -> [(FastString, LPat GhcPs)]
  -> Writer Any (LPat GhcPs)
mkRecPat l = \case
  [] -> do
      return (patLoc l (BangPat EpAnnNotUsed (patLoc l (WildPat defExt))))
  [(f, p)] -> do
    doImport
    return (patLoc l (ViewPat EpAnnNotUsed (mkGetField f) p))
  fields -> do
    doImport  
    let x  = mkRdrUnqual $ mkVarOcc "x"
    let getFieldsTuple = simpleLam x (mkTuple [mkGetField f `mkHsApp` mkVar l x | (f, _) <- fields])
    let patsTuple = TuplePat EpAnnNotUsed [p | (_, p) <- fields] Boxed
    return (patLoc l (ViewPat EpAnnNotUsed getFieldsTuple (patLoc l patsTuple)))
  where
    doImport :: Writer Any ()
    doImport = tell (Any True)

    mkGetField :: FastString -> LHsExpr GhcPs
    mkGetField fieldName =
      mkVar l getField' `mkAppType` mkSelector fieldName

    getField' = mkRdrQual ghcRecordsCompat $ mkVarOcc "getField"

    mkSelector :: FastString -> LHsType GhcPs
    mkSelector = litT . HsStrTy NoSourceText 

    mkTuple :: [LHsExpr GhcPs] -> LHsExpr GhcPs
    mkTuple xs = L (mkSrcSpanAnn l) (ExplicitTuple EpAnnNotUsed [(Present EpAnnNotUsed x) | x <- xs] Boxed)

ghcRecordsCompat = mkModuleName "GHC.Records.Compat"

getFieldSel :: LHsRecField GhcPs (LPat GhcPs) -> Maybe (FastString, LPat GhcPs)
getFieldSel (L _ (HsRecField _ (L _ fieldOcc) arg pun))
  | FieldOcc _ (L l nm) <- fieldOcc
  , Unqual nm' <- nm
  = Just (occNameFS nm', if pun then nlVarPat nm  else arg)

  | otherwise
  = Nothing

{-------------------------------------------------------------------------------
  Check for enabled extensions

  In ghc 8.10 and up there are DynFlags plugins, which we could use to enable
  these extensions for the user. Since this is not available in 8.8 however we
  will not make use of this for now. (There is also reason to believe that these
  may be removed again in later ghc releases.)
-------------------------------------------------------------------------------}

checkEnabledExtensions :: SrcSpan -> Hsc ()
checkEnabledExtensions l = do
    dynFlags <- getDynFlags
    let missing :: [RequiredExtension]
        missing = filter (not . isEnabled dynFlags) requiredExtensions
    unless (null missing) $
      -- We issue a warning here instead of an error, for better integration
      -- with HLS. Frankly, I'm not entirely sure what's going on there.
      issueWarning l $ vcat . concat $ [
          [text "Please enable these extensions for use with Nau.PLugin.ViewPattern:"]
        , map ppr missing
        ]
  where
    requiredExtensions :: [RequiredExtension]
    requiredExtensions = [
          RequiredExtension [DataKinds]
        , RequiredExtension [FlexibleContexts]
        , RequiredExtension [TypeApplications]
        , RequiredExtension [ViewPatterns]
        ]

-- | Required extension
--
-- The list is used to represent alternative extensions that could all work
-- (e.g., @GADTs@ and @ExistentialQuantification@).
data RequiredExtension = RequiredExtension [Extension]

instance Outputable RequiredExtension where
  ppr (RequiredExtension exts) = hsep . intersperse (text "or") $ map ppr exts

isEnabled :: DynFlags -> RequiredExtension -> Bool
isEnabled dynflags (RequiredExtension exts) = any (`xopt` dynflags) exts

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Lib.litT'
litT :: HsTyLit -> LHsType GhcPs
litT = noLocA . HsTyLit defExt

-- | Construct simple lambda
--
-- Constructs lambda of the form
--
-- > \x -> e
simpleLam :: RdrName -> LHsExpr GhcPs -> LHsExpr GhcPs
simpleLam x body = mkHsLam [nlVarPat x] body

mkVar :: SrcSpan -> RdrName -> LHsExpr GhcPs
mkVar l name = L (mkSrcSpanAnn l) $ HsVar defExt (L (SrcSpanAnn EpAnnNotUsed l) name)

mkAppType :: LHsExpr GhcPs -> LHsType GhcPs -> LHsExpr GhcPs
mkAppType expr typ = noLocA $ HsAppType noSrcSpan expr (HsWC NoExtField typ)

issueWarning :: SrcSpan -> SDoc -> Hsc ()
issueWarning l errMsg = do
  dynFlags <- getDynFlags
  logger <- getLogger
  liftIO $ printOrThrowWarnings logger dynFlags . listToBag . (:[]) $
    mkWarnMsg l neverQualify errMsg
