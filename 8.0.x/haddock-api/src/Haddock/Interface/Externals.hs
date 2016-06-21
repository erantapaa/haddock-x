
module Haddock.Interface.Externals
  (ghcBuildExternsMap)
where

import Haddock.Types

import Avail (AvailInfo(..), availsToNameSetWithSelectors, availName)
import GHC
import FieldLabel (FieldLbl(..))
import IOEnv (readMutVar, liftIO)
import ListSetOps (unionLists)
import Maybes (orElse)
import NameEnv (emptyNameEnv, nameEnvElts, extendNameEnv_C)
import NameSet (NameSet, elemNameSet, emptyNameSet, extendNameSet, mkNameSet, nameSetElems)
import Outputable (pprPanic, ppr, hsep)
import RdrName (GlobalRdrElt(..), ImportSpec(..), availFromGRE, bestImport, is_dloc)
import TcRnTypes (RnM, tcg_rn_imports, tcg_used_gres)
import TcRnMonad (initTcForLookup )

import qualified Data.Map as Map
import Data.Map (Map)

type ImportDeclUsage
   = ( LImportDecl Name   -- The import declaration
     , [AvailInfo]        -- What *is* used (normalised)
     , [Name] )           -- What is imported but *not* used

type ImportMap = Map SrcLoc [AvailInfo]  -- See [The ImportMap]

toNames :: AvailInfo -> [Name]
toNames (Avail _ x) = [x]
toNames (AvailTC x xs _) = (x:xs)

debugIsOn :: Bool
debugIsOn = False

ghcBuildExternsMap :: TypecheckedModule -> Ghc ExternsMap
ghcBuildExternsMap tm = do
  hsc_env <- getSession
  liftIO $ initTcForLookup hsc_env (buildExternsMap tm)

buildExternsMap :: TypecheckedModule -> RnM ExternsMap
buildExternsMap tcm = do
  let (gbl_env, _) = tm_internals_ tcm
      all_imports = tcg_rn_imports gbl_env
  uses <- readMutVar (tcg_used_gres gbl_env)
  let usage = findImportUsage all_imports uses

      pairs = [ (name, ldecl)
                | (ldecl, avails, _) <- usage
                , avail <- avails
                , name <- toNames avail
              ]
  return $ Map.fromList pairs

-- copied from RnNames

-- | Combines 'AvailInfo's from the same family
-- 'avails' may have several items with the same availName
-- E.g  import Ix( Ix(..), index )
-- will give Ix(Ix,index,range) and Ix(index)
-- We want to combine these; addAvail does that
nubAvails :: [AvailInfo] -> [AvailInfo]
nubAvails avails = nameEnvElts (foldl add emptyNameEnv avails)
  where
    add env avail = extendNameEnv_C plusAvail env (availName avail) avail

findImportUsage :: [LImportDecl Name]
                -> [GlobalRdrElt]
                -> [ImportDeclUsage]

findImportUsage imports used_gres
  = map unused_decl imports
  where
    import_usage :: ImportMap
    import_usage
      = foldr extendImportMap Map.empty used_gres

    unused_decl decl@(L loc (ImportDecl { ideclHiding = imps }))
      = (decl, nubAvails used_avails, nameSetElems unused_imps)
      where
        used_avails = Map.lookup (srcSpanEnd loc) import_usage `orElse` []
                      -- srcSpanEnd: see Note [The ImportMap]
        used_names   = availsToNameSetWithSelectors used_avails
        used_parents = mkNameSet [n | AvailTC n _ _ <- used_avails]

        unused_imps   -- Not trivial; see eg Trac #7454
          = case imps of
              Just (False, L _ imp_ies) ->
                                 foldr (add_unused . unLoc) emptyNameSet imp_ies
              _other -> emptyNameSet -- No explicit import list => no unused-name list

        add_unused :: IE Name -> NameSet -> NameSet
        add_unused (IEVar (L _ n))      acc = add_unused_name n acc
        add_unused (IEThingAbs (L _ n)) acc = add_unused_name n acc
        add_unused (IEThingAll (L _ n)) acc = add_unused_all  n acc
        add_unused (IEThingWith (L _ p) wc ns fs) acc =
          add_wc_all (add_unused_with p xs acc)
          where xs = map unLoc ns ++ map (flSelector . unLoc) fs
                add_wc_all = case wc of
                            NoIEWildcard -> id
                            IEWildcard _ -> add_unused_all p
        add_unused _ acc = acc

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

extendImportMap :: GlobalRdrElt -> ImportMap -> ImportMap
-- For each of a list of used GREs, find all the import decls that brought
-- it into scope; choose one of them (bestImport), and record
-- the RdrName in that import decl's entry in the ImportMap
extendImportMap gre imp_map
   = add_imp gre (bestImport (gre_imp gre)) imp_map
  where
    add_imp :: GlobalRdrElt -> ImportSpec -> ImportMap -> ImportMap
    add_imp gre' (ImpSpec { is_decl = imp_decl_spec }) imp_map'
      = Map.insertWith add decl_loc [avail] imp_map'
      where
        add _ avails = avail : avails -- add is really just a specialised (++)
        decl_loc = srcSpanEnd (is_dloc imp_decl_spec)
                   -- For srcSpanEnd see Note [The ImportMap]
        avail    = availFromGRE gre'

plusAvail :: AvailInfo -> AvailInfo -> AvailInfo
plusAvail a1 a2
  | debugIsOn && availName a1 /= availName a2
  = pprPanic "RnEnv.plusAvail names differ" (hsep [ppr a1,ppr a2])
plusAvail a1@(Avail {})         (Avail {})        = a1
plusAvail (AvailTC _ [] [])     a2@(AvailTC {})   = a2
plusAvail a1@(AvailTC {})       (AvailTC _ [] []) = a1
plusAvail (AvailTC n1 (s1:ss1) fs1) (AvailTC n2 (s2:ss2) fs2)
  = case (n1==s1, n2==s2) of  -- Maintain invariant the parent is first
       (True,True)   -> AvailTC n1 (s1 : (ss1 `unionLists` ss2))
                                   (fs1 `unionLists` fs2)
       (True,False)  -> AvailTC n1 (s1 : (ss1 `unionLists` (s2:ss2)))
                                   (fs1 `unionLists` fs2)
       (False,True)  -> AvailTC n1 (s2 : ((s1:ss1) `unionLists` ss2))
                                   (fs1 `unionLists` fs2)
       (False,False) -> AvailTC n1 ((s1:ss1) `unionLists` (s2:ss2))
                                   (fs1 `unionLists` fs2)
plusAvail (AvailTC n1 ss1 fs1) (AvailTC _ [] fs2)
  = AvailTC n1 ss1 (fs1 `unionLists` fs2)
plusAvail (AvailTC n1 [] fs1)  (AvailTC _ ss2 fs2)
  = AvailTC n1 ss2 (fs1 `unionLists` fs2)
plusAvail a1 a2 = pprPanic "RnEnv.plusAvail" (hsep [ppr a1,ppr a2])

