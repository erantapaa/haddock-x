
module Haddock.Interface.Externals (
  ghcBuildExternsMap
) where

import Haddock.Types

import GHC
import Avail
-- import FastString
-- import HsImpExp    -- Haskell Import / Export
import ListSetOps
import Maybes
import Name
import NameEnv
import NameSet
import RdrName
import TcRnMonad
-- import TcRnTypes
import Outputable
import Util

import Data.List        ( partition) -- , (\\), find )
import Data.Map         ( Map )
import qualified Data.Map as Map
import qualified Data.Set as Set

-- imports needed for printMinimalImports
-- import DynFlags
-- import LoadIface
-- import Module
-- import System.FilePath  ((</>))
-- import System.IO

type ImportDeclUsage
   = ( LImportDecl Name   -- The import declaration
     , [AvailInfo]        -- What *is* used (normalised)
     , [Name] )           -- What is imported but *not* used

type ImportMap = Map SrcLoc [AvailInfo]  -- See [The ImportMap]

ghcBuildExternsMap :: TypecheckedModule -> Ghc ExternsMap
ghcBuildExternsMap tm = do
  hsc_env <- getSession
  liftIO $ initTcForLookup hsc_env (buildExternsMap tm)

buildExternsMap :: TypecheckedModule -> RnM ExternsMap
buildExternsMap tm = do
  let (gbl_env, _) = tm_internals_ tm
      all_imports = tcg_rn_imports gbl_env
      rdr_env = tcg_rdr_env gbl_env
  uses <- readMutVar (tcg_used_rdrnames gbl_env)
  let usage = findImportUsage all_imports rdr_env (Set.elems uses)

      pairs = [ (name, ldecl)
                | (ldecl, avails, _) <- usage
                , avail <- avails
                , name <- toNames avail
              ]
  return $ Map.fromList pairs

toNames :: AvailInfo -> [Name]
toNames (Avail x) = [x]
toNames (AvailTC x xs) = (x:xs)

{- unused
fmtImport :: ImportDecl Name -> SDoc
fmtImport imp
  = text "import " <> qual (ideclPkgQual imp) <> ppr (unLoc (ideclName imp)) <> as (ideclAs imp)
  where qual Nothing   = empty
        qual (Just fs) = doubleQuotes (ppr fs) <> text " "
        as Nothing     = empty
        as (Just m)    = text " as " <> ppr m
-}

---------------------------------

findImportUsage :: [LImportDecl Name]
                -> GlobalRdrEnv
                -> [RdrName]
                -> [ImportDeclUsage]

findImportUsage imports rdr_env rdrs
  = map unused_decl imports
  where
    import_usage :: ImportMap
    import_usage = foldr (extendImportMap rdr_env) Map.empty rdrs

    unused_decl decl@(L loc (ImportDecl { ideclHiding = imps }))
      = (decl, nubAvails used_avails, nameSetElems unused_imps)
      where
        used_avails = Map.lookup (srcSpanEnd loc) import_usage `orElse` []
                      -- srcSpanEnd: see Note [The ImportMap]
        used_names   = availsToNameSet used_avails
        used_parents = mkNameSet [n | AvailTC n _ <- used_avails]

        unused_imps   -- Not trivial; see eg Trac #7454
          = case imps of
              Just (False, L _ imp_ies) ->
                                 foldr (add_unused . unLoc) emptyNameSet imp_ies
              _other -> emptyNameSet -- No explicit import list => no unused-name list

        add_unused :: IE Name -> NameSet -> NameSet
        add_unused (IEVar (L _ n))      acc = add_unused_name n acc
        add_unused (IEThingAbs (L _ n)) acc = add_unused_name n acc
        add_unused (IEThingAll (L _ n)) acc = add_unused_all  n acc
        add_unused (IEThingWith (L _ p) ns) acc
                                          = add_unused_with p (map unLoc ns) acc
        add_unused _                    acc = acc

        add_unused_name n acc
          | n `elemNameSet` used_names = acc
          | otherwise                  = acc `extendNameSet` n
        add_unused_all n acc
          | n `elemNameSet` used_names   = acc
          | n `elemNameSet` used_parents = acc
          | otherwise                    = acc `extendNameSet` n
        add_unused_with p ns acc
          | all (`elemNameSet` acc1) ns = add_unused_name p acc1
          | otherwise = acc1
          where
            acc1 = foldr add_unused_name acc ns
       -- If you use 'signum' from Num, then the user may well have
       -- imported Num(signum).  We don't want to complain that
       -- Num is not itself mentioned.  Hence the two cases in add_unused_with.

nubAvails :: [AvailInfo] -> [AvailInfo]
nubAvails avails = nameEnvElts (foldl add emptyNameEnv avails)
  where
    add env avail = extendNameEnv_C plusAvail env (availName avail) avail

plusAvail :: AvailInfo -> AvailInfo -> AvailInfo
plusAvail a1 a2
  | debugIsOn && availName a1 /= availName a2
  = pprPanic "RnEnv.plusAvail names differ" (hsep [ppr a1,ppr a2])
plusAvail a1@(Avail {})         (Avail {})      = a1
plusAvail (AvailTC _ [])        a2@(AvailTC {}) = a2
plusAvail a1@(AvailTC {})       (AvailTC _ [])  = a1
plusAvail (AvailTC n1 (s1:ss1)) (AvailTC n2 (s2:ss2))
  = case (n1==s1, n2==s2) of  -- Maintain invariant the parent is first
       (True,True)   -> AvailTC n1 (s1 : (ss1 `unionLists` ss2))
       (True,False)  -> AvailTC n1 (s1 : (ss1 `unionLists` (s2:ss2)))
       (False,True)  -> AvailTC n1 (s2 : ((s1:ss1) `unionLists` ss2))
       (False,False) -> AvailTC n1 ((s1:ss1) `unionLists` (s2:ss2))
plusAvail a1 a2 = pprPanic "RnEnv.plusAvail" (hsep [ppr a1,ppr a2])

extendImportMap :: GlobalRdrEnv -> RdrName -> ImportMap -> ImportMap
-- For a used RdrName, find all the import decls that brought
-- it into scope; choose one of them (bestImport), and record
-- the RdrName in that import decl's entry in the ImportMap
extendImportMap rdr_env rdr imp_map
  | [gre] <- lookupGRE_RdrName rdr rdr_env
  , Imported imps <- gre_prov gre
  = add_imp gre (bestImport imps) imp_map
  | otherwise
  = imp_map
  where
    add_imp :: GlobalRdrElt -> ImportSpec -> ImportMap -> ImportMap
    add_imp gre (ImpSpec { is_decl = imp_decl_spec }) imp_map'
      = Map.insertWith add decl_loc [avail] imp_map'
      where
        add _ avails = avail : avails -- add is really just a specialised (++)
        decl_loc = srcSpanEnd (is_dloc imp_decl_spec)
                   -- For srcSpanEnd see Note [The ImportMap]
        avail    = greExportAvail gre

    bestImport :: [ImportSpec] -> ImportSpec
    bestImport iss
      = case partition isImpAll iss of
          ([], imp_somes) -> textuallyFirst imp_somes
          (imp_alls, _)   -> textuallyFirst imp_alls

    textuallyFirst :: [ImportSpec] -> ImportSpec
    textuallyFirst iss = case sortWith (is_dloc . is_decl) iss of
                           []     -> pprPanic "textuallyFirst" (ppr iss)
                           (is:_) -> is

    isImpAll :: ImportSpec -> Bool
    isImpAll (ImpSpec { is_item = ImpAll }) = True
    isImpAll _other                         = False

greExportAvail :: GlobalRdrElt -> AvailInfo
greExportAvail gre
  = case gre_par gre of
      ParentIs p                  -> AvailTC p [me]
      NoParent   | isTyConName me -> AvailTC me [me]
                 | otherwise      -> Avail   me
  where
    me = gre_name gre

{- unused
printMinimalImports :: [ImportDeclUsage] -> RnM ()
-- See Note [Printing minimal imports]
printMinimalImports imports_w_usage
  = do { imports' <- mapM mk_minimal imports_w_usage
       ; this_mod <- getModule
       ; dflags   <- getDynFlags
       ; liftIO $
         do { h <- openFile (mkFilename dflags this_mod) WriteMode
            ; printForUser dflags h neverQualify (vcat (map ppr imports')) }
              -- The neverQualify is important.  We are printing Names
              -- but they are in the context of an 'import' decl, and
              -- we never qualify things inside there
              -- E.g.   import Blag( f, b )
              -- not    import Blag( Blag.f, Blag.g )!
       }

  where
    mkFilename dflags this_mod
      | Just d <- dumpDir dflags = d </> basefn
      | otherwise                = basefn
      where
        basefn = moduleNameString (moduleName this_mod) ++ ".imports"

    mk_minimal (L l decl, used, unused)
      | null unused
      , Just (False, _) <- ideclHiding decl
      = return (L l decl)
      | otherwise
      = do { let ImportDecl { ideclName    = L _ mod_name
                            , ideclSource  = is_boot
                            , ideclPkgQual = mb_pkg } = decl
           ; ifaces <- loadSrcInterface doc mod_name is_boot mb_pkg
           ; let lies = map (L l) (concatMap (to_ie ifaces) used)
           ; return (L l (decl { ideclHiding = Just (False, L l lies) })) }
      where
        doc = text "Compute minimal imports for" <+> ppr decl

    to_ie :: [ModIface] -> AvailInfo -> [IE Name]
    -- The main trick here is that if we're importing all the constructors
    -- we want to say "T(..)", but if we're importing only a subset we want
    -- to say "T(A,B,C)".  So we have to find out what the module exports.
    to_ie _ (Avail n)
       = [IEVar (noLoc n)]
    to_ie _ (AvailTC n [m])
       | n==m = [IEThingAbs (noLoc n)]
    to_ie ifaces (AvailTC n ns)
      = case [xs | iface <- ifaces
                 , AvailTC x xs <- mi_exports iface
                 , x == n
                 , x `elem` xs    -- Note [Partial export]
                 ] of
           [xs] | all_used xs -> [IEThingAll (noLoc n)]
                | otherwise   -> [IEThingWith (noLoc n)
                                              (map noLoc (filter (/= n) ns))]
           _other             -> map (IEVar . noLoc)  ns
        where
          all_used avail_occs = all (`elem` ns) avail_occs
-}
