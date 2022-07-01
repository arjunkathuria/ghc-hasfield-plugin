{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Nau.Plugin.ViewPattern (plugin) where

import Data.Generics (everywhereM, mkM)    
import Nau.Plugin.Shim 
import Nau.Plugin.NamingT

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
      (decls', modls) <- runNamingHsc $ everywhereM (mkM transformPat) decls
      return $ parsed {
        hpm_module = L l $ modl {
            hsmodDecls   = decls'
          , hsmodImports = imports -- ++ modls
--          , hsmodImports = imports ++ map (importDecl True) modls
          }
        }

{-------------------------------------------------------------------------------
  Main translation
-------------------------------------------------------------------------------}

transformPat :: LPat GhcPs -> NamingT Hsc (LPat GhcPs)
transformPat p
  | Just (L l nm, RecCon (HsRecFields flds dotdot)) <- viewConPat p
  , Unqual nm' <- nm
  , Nothing    <- dotdot
  , Just flds' <- mapM getFieldSel flds
  =  mkRecPat l flds'

  | otherwise
  = return p

mkRecPat ::
     SrcSpan
  -> [(FastString, LPat GhcPs)]
  -> NamingT Hsc (LPat GhcPs)
mkRecPat l = \case
  [] -> do
      return (patLoc l (BangPat defExt (WildPat defExt)))
  [(f, p)] -> do
    x <- freshVar l "x" 
    return (patLoc l (ViewPat defExt (mkGetField f) p))
  fields -> do
    x <- freshVar l "x" 
    let getFieldsTuple = simpleLam x (mkTuple [mkGetField f `mkHsApp` mkVar l x | (f, _) <- fields])
    let patsTuple = TuplePat defExt [p | (_, p) <- fields] Boxed
    return (patLoc l (ViewPat defExt getFieldsTuple (patLoc l patsTuple)))
  where
    mkGetField :: FastString -> LHsExpr GhcPs
    mkGetField fieldName =
      mkVar l getField' `mkAppType` mkSelector fieldName

    getField' = mkRdrQual (mkModuleName "GHC.Records.Compat") $ mkVarOcc "getField"

    mkSelector :: FastString -> LHsType GhcPs
    mkSelector = litT . HsStrTy NoSourceText 

    mkTuple :: [LHsExpr GhcPs] -> LHsExpr GhcPs
    mkTuple xs = L l (ExplicitTuple defExt [L l (Present defExt x) | x <- xs] Boxed)

getFieldSel :: LHsRecField GhcPs (LPat GhcPs) -> Maybe (FastString, LPat GhcPs)
getFieldSel (L _ (HsRecField (L _ fieldOcc) arg pun))
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
litT = noLoc . HsTyLit defExt

-- | Construct simple lambda
--
-- Constructs lambda of the form
--
-- > \x -> e
simpleLam :: RdrName -> LHsExpr GhcPs -> LHsExpr GhcPs
simpleLam x body = mkHsLam [nlVarPat x] body

mkVar :: SrcSpan -> RdrName -> LHsExpr GhcPs
mkVar l name = L l $ HsVar defExt (L l name)

mkAppType :: LHsExpr GhcPs -> LHsType GhcPs -> LHsExpr GhcPs
mkAppType expr typ = noLoc $ HsAppType defExt expr (HsWC defExt typ)

issueWarning :: SrcSpan -> SDoc -> Hsc ()
issueWarning l errMsg = do
  dynFlags <- getDynFlags
  liftIO $ printOrThrowWarnings dynFlags . listToBag . (:[]) $
    mkWarnMsg dynFlags l neverQualify errMsg