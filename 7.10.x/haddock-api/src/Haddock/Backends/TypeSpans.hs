{-# LANGUAGE GADTs, RankNTypes, PatternGuards, TupleSections #-}

module Haddock.Backends.TypeSpans
where

import GhcMonad (Ghc, liftIO)
import GHC

import CoreUtils
import CoreSyn
import Desugar  -- desugarExpr
import HsExpr -- LHsExpr, mg_arg_tys, mg_res_ty
import Outputable -- PprStyle
import PprTyThing -- pprTypeForUser
import Pretty -- Mode(..)
import TcHsSyn -- hsPatType
import Type -- mkFunTys

--
import Haddock.Types
import Haddock.Backends.Hyperlinker.Utils (hypSrcDir)

--
import Data.Maybe (isJust)
import System.IO
import Control.Monad
import System.FilePath
import Data.List
import Data.Function (on)
import Data.Ord as O
import Data.Maybe
import Text.Printf
import Data.Char

-- Generics:

import Data.Data
import Data.Generics.Aliases hiding (GT)

-- Collect all of the spans with types

ppCollectTypedNodes :: TypecheckedSource -> Module -> ErrMsgGhc TypedNodes
ppCollectTypedNodes tc_src mod = do
  let modl = moduleNameString $ moduleName mod
  let lexprs = findLocated tc_src :: [LHsExpr Id]
      lbinds = findLocated tc_src :: [LHsBind Id]
      lpats  = findLocated tc_src :: [LPat Id]
      msg = modl ++ ": exprs: " ++ show (length lexprs)
                 ++ " binds: " ++ show (length lbinds)
                 ++ " pats: " ++ show (length lpats)

  liftErrMsg $ tell [ "=== " ++ msg ]
  return (lexprs, lbinds, lpats)

-- generate type spans for a single Interface
genTypeSpans :: DynFlags -> Interface -> IO (String, [(Int,Int,Int,Int,String)])
genTypeSpans dflags iface = do
  let (lexprs,lbinds,lpats) = ifaceTypedNodes iface
      hs_env = ifaceHscEnv iface
      modname = moduleNameString $ moduleName $ ifaceMod iface
      style = defaultUserStyle

  exprs <- mapM (getType hs_env) lexprs
  binds <- mapM (getTypeBind hs_env) lbinds
  pats  <- mapM (getTypePat hs_env) lpats

  let pairs = catMaybes (exprs ++ binds ++ pats)
      sorted :: [ (SrcSpan, Type) ]
      sorted = sortBy (comparing (fst4.fourInts.fst)) pairs
      tuples  = map (toTuple dflags style) sorted
  return (modname, tuples)

ppEmitTypeSpans :: DynFlags -> [Interface] -> FilePath -> FilePath -> IO ()
ppEmitTypeSpans dflags ifaces outdir outfile = do
  let outpath = outdir </> hypSrcDir </> outfile
      style = defaultUserStyle

  withFile outpath WriteMode $ \h -> do
    let emit = hPutStrLn h
    forM_ ifaces $ \iface -> do
      (modname, tuples) <- genTypeSpans dflags iface
      emit $ "module " ++ modname
      forM_ tuples $ \(a,b,c,d,t) -> do
        emit $ intercalate " " [show a, show b, show c, show d, t]

jslist (a,b,c,d,t) = "[" ++ intercalate "," [show a, show b, show c, show d, jsstr t] ++ "]"

-- simple conversion routines to avoid bringing in Aeson
jsstr :: String -> String
jsstr s = "\"" ++ concatMap jschr s ++ "\""
jschr :: Char -> String
jschr ch
  | ch == '"'  = "\\\""
  | ch == '\\' = "\\\\"
  | isAscii ch && isPrint ch = [ch]
  | n < 256    = "\\x" ++ printf "%02x" n
  | otherwise  = "\\u" ++ printf "%04x" n
  where n = ord ch

-- emit the type spans as JS in separate files
ppEmitTypeSpansJSON :: DynFlags -> [Interface] -> FilePath -> IO ()
ppEmitTypeSpansJSON dflags ifaces outdir = do
  forM_ ifaces $ \iface -> do
    (modname, tuples) <- genTypeSpans dflags iface
    let outpath = outdir </> hypSrcDir </> (modname ++ ".json")
        output = "[\n"
                   ++ (intercalate ",\n" $ map jslist tuples)
                   ++ "\n]\n"
    withFile outpath WriteMode $ \h -> do
      hPutStrLn h output

-- ------ Rendering a SrcSpan

fst4 :: (a,b,c,d) -> a
fst4 (a,b,c,d) = a

cmp :: SrcSpan -> SrcSpan -> Ordering
cmp a b
  | a `isSubspanOf` b = O.LT
  | b `isSubspanOf` a = O.GT
  | otherwise         = O.EQ

fourInts :: SrcSpan -> (Int,Int,Int,Int)
fourInts = fromMaybe (0,0,0,0) . getSrcSpan

getSrcSpan :: SrcSpan -> Maybe (Int,Int,Int,Int)
getSrcSpan (RealSrcSpan spn)
             = Just (srcSpanStartLine spn
                   , srcSpanStartCol spn
                   , srcSpanEndLine spn
                   , srcSpanEndCol spn)
getSrcSpan _ = Nothing

-- ------ convert Type to String

showDocWith :: DynFlags -> Pretty.Mode -> Pretty.Doc -> String
showDocWith dflags mode = Pretty.showDoc mode (pprCols dflags)

showOneLine :: DynFlags -> PprStyle -> SDoc -> String
showOneLine dflag style = showDocWith dflag OneLineMode . withPprStyleDoc dflag style

pretty :: DynFlags -> PprStyle -> Type -> String
pretty dflag style = showOneLine dflag style . pprTypeForUser

toTup :: DynFlags -> PprStyle -> (SrcSpan,Type) -> String
toTup dflag style (span,typ) =
   let (a,b,c,d) = fourInts span
       t = pretty dflag style typ
   in intercalate " " (map show [a,b,c,d] ++ [t])

toTuple :: DynFlags -> PprStyle -> (SrcSpan, Type) -> (Int,Int,Int,Int,String)
toTuple dflag style (span,typ) =
   let (a,b,c,d) = fourInts span
       t = pretty dflag style typ
   in (a,b,c,d,t)

-- ------ getType

deSugar' :: LHsExpr Id -> HscEnv -> IO (Maybe CoreExpr)
deSugar' e hs_env = snd <$> deSugarExpr hs_env e

getType :: HscEnv -> LHsExpr Id -> IO (Maybe (SrcSpan, Type))
getType hs_env e = do
    mbe <- deSugar' e hs_env
    return $ (GHC.getLoc e, ) <$> CoreUtils.exprType <$> mbe


getTypeBind :: HscEnv -> LHsBind Id -> IO (Maybe (SrcSpan, Type))
getTypeBind _ (L spn FunBind{fun_matches = m}) = return $ Just (spn, typ)
  where in_tys = mg_arg_tys m
        out_typ = mg_res_ty m
        typ = mkFunTys in_tys out_typ
getTypeBind _ _ = return Nothing

getTypePat :: HscEnv -> LPat Id -> IO (Maybe (SrcSpan, Type))
getTypePat _ (L spn pat) = return $ Just (spn, hsPatType pat)

-- ------

findLocated :: (Data a, Typeable b) => a -> [Located b]
findLocated a = listifyBut (isGoodSrcSpan . getLoc) [] a

-- findLPats :: Data a => a -> [LPat Id]
-- findLPats a = listifyBut (isGoodSrcSpan . getLoc) [] a

toRealSrcSpan :: SrcSpan -> Maybe RealSrcSpan
toRealSrcSpan (UnhelpfulSpan _) = Nothing
toRealSrcSpan (RealSrcSpan sp)  = Just sp


data Guard where
  Guard :: Typeable a => Maybe a -> Guard

isPost :: Typeable a => a -> [Guard] -> Bool
isPost a = or . map check
  where check :: Guard -> Bool
        check x = case x of
                    Guard y -> isJust $ (cast a) `asTypeOf` y

-- | Summarise all nodes in top-down, left-to-right order
everythingButQ :: (r -> r -> r) -> [Guard] -> GenericQ r -> GenericQ r
everythingButQ k q f x
  = foldl k (f x) fsp
    where fsp = case isPost x q of
                  True  -> []
                  False -> gmapQ (everythingButQ k q f) x

-- | Get a list of all entities that meet a predicate
listifyBut :: Typeable r => (r -> Bool) -> [Guard] -> GenericQ [r]
listifyBut p q
  = everythingButQ (++) q ([] `mkQ` (\x -> if p x then [x] else []))

