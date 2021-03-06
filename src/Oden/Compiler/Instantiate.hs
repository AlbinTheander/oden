-- | Instantiates polymorphically typed expressions with monomorphic types.
module Oden.Compiler.Instantiate (
  instantiate,
  InstantiateError(..)
) where

import           Control.Monad.Except
import           Control.Monad.State

import qualified Data.Map              as Map hiding (foldl)

import qualified Oden.Core             as Core
import           Oden.Metadata
import           Oden.SourceInfo
import qualified Oden.Type.Monomorphic as Mono
import qualified Oden.Type.Polymorphic as Poly

data InstantiateError = TypeMismatch SourceInfo Poly.Type Mono.Type
                      | SubstitutionFailed SourceInfo Poly.TVar [Poly.TVar]
                      deriving (Show, Eq, Ord)

type Substitutions = Map.Map Poly.TVar Poly.Type
type Instantiate a = StateT Substitutions (Except InstantiateError) a

monoToPoly :: Mono.Type -> Poly.Type
monoToPoly (Mono.TTuple si f s r) =
  Poly.TTuple si (monoToPoly f) (monoToPoly s) (map monoToPoly r)
monoToPoly (Mono.TCon si n) = Poly.TCon si n
monoToPoly (Mono.TNoArgFn si f) = Poly.TNoArgFn si (monoToPoly f)
monoToPoly (Mono.TFn si f p) = Poly.TFn si (monoToPoly f) (monoToPoly p)
monoToPoly (Mono.TForeignFn si variadic as r) =
  Poly.TForeignFn si variadic (map monoToPoly as) (map monoToPoly r)
monoToPoly (Mono.TSlice si t) = Poly.TSlice si (monoToPoly t)
monoToPoly (Mono.TRecord si row) = Poly.TRecord si (monoToPoly row)
monoToPoly (Mono.TNamed si n t) = Poly.TNamed si n (monoToPoly t)
monoToPoly (Mono.REmpty si) = Poly.REmpty si
monoToPoly (Mono.RExtension si label polyType row) =
  Poly.RExtension si label (monoToPoly polyType) (monoToPoly row)

getSubstitutions :: Poly.Type -> Mono.Type -> Either InstantiateError Substitutions
getSubstitutions (Poly.TCon _ pn) (Mono.TCon _ mn)
  | pn == mn = Right Map.empty
getSubstitutions (Poly.TNoArgFn _ pf) (Mono.TNoArgFn _ mf) =
  getSubstitutions pf mf
getSubstitutions (Poly.TFn _ pf pp) (Mono.TFn _ mf mp) = do
  fs <- getSubstitutions pf mf
  ps <- getSubstitutions pp mp
  return (fs `mappend` ps)
getSubstitutions (Poly.TVar _ v) mono = Right (Map.singleton v (monoToPoly mono))
getSubstitutions (Poly.TForeignFn _ _ pas prs) (Mono.TForeignFn _ _ mas mrs) = do
  as <- zipWithM getSubstitutions pas mas
  rs <- zipWithM getSubstitutions prs mrs
  return (mconcat (as ++ rs))
getSubstitutions (Poly.TTuple _ pf ps pr) (Mono.TTuple _ mf ms mr) = do
  f <- getSubstitutions pf mf
  s <- getSubstitutions ps ms
  r <- zipWithM getSubstitutions pr mr
  return (foldl mappend f (s:r))
getSubstitutions (Poly.TSlice _ p) (Mono.TSlice _ m) =
  getSubstitutions p m

getSubstitutions (Poly.TRecord _ polyRow) (Mono.TRecord _ monoRow) =
  getSubstitutions polyRow monoRow

getSubstitutions polyRow@(Poly.RExtension (Metadata _psi) _ _ _) monoRow@(Mono.RExtension (Metadata msi) _ _ _) = do

  -- Extract the fields in each rows.
  let polyFields = Map.fromList (Poly.rowToList polyRow)
      monoFields = Map.fromList (Mono.rowToList monoRow)
      onlyInPoly = polyFields `Map.difference` monoFields
      onlyInMono = monoFields `Map.difference` polyFields

  unless (Map.null onlyInPoly) $
    throwError (TypeMismatch msi polyRow monoRow)

  -- Get substitutions the common fields.
  substs <- sequence (Map.elems (Map.intersectionWith getSubstitutions polyFields monoFields))

  -- Retrieve leaf rows.
  let polyLeaf = Poly.getLeafRow polyRow

  -- Only the polymorphic leaf (possible a row variable) with the remaining
  -- monomorphic fields.
  leafSubst <- getSubstitutions polyLeaf (Mono.rowFromList $ Map.assocs onlyInMono)

  return (foldl mappend leafSubst substs)

getSubstitutions Poly.REmpty{} Mono.REmpty{} = return Map.empty
getSubstitutions poly mono = throwError (TypeMismatch (getSourceInfo mono) poly mono)

replace :: Poly.Type -> Instantiate Poly.Type
replace (Poly.TTuple si f s r) =
  Poly.TTuple si <$> replace f <*> replace s <*> mapM replace r
replace (Poly.TVar (Metadata si) v) = do
  s <- get
  case Map.lookup v s of
    Just mono -> return (setSourceInfo si mono)
    Nothing -> throwError (SubstitutionFailed si v (Map.keys s))
replace (Poly.TCon si n) = return (Poly.TCon si n)
replace (Poly.TNoArgFn si t) = Poly.TNoArgFn si <$> replace t
replace (Poly.TFn si ft pt) = Poly.TFn si <$> replace ft <*> replace pt
replace (Poly.TForeignFn si variadic ft pt) =
  Poly.TForeignFn si variadic <$> mapM replace ft <*> mapM replace pt
replace (Poly.TSlice si t) = Poly.TSlice si <$> replace t
replace (Poly.TRecord si r) = Poly.TRecord si <$> replace r
replace (Poly.TNamed si n t) = Poly.TNamed si n <$> replace t
replace (Poly.REmpty si) = return $ Poly.REmpty si
replace (Poly.RExtension si label type' row) =
  Poly.RExtension si label <$> replace type'
                           <*> replace row

instantiateField :: Core.FieldInitializer Poly.Type
                -> Instantiate (Core.FieldInitializer Poly.Type)
instantiateField (Core.FieldInitializer si label expr) =
  Core.FieldInitializer si label <$> instantiateExpr expr

instantiateExpr :: Core.Expr Poly.Type
                -> Instantiate (Core.Expr Poly.Type)
instantiateExpr (Core.Symbol si i t) =
  Core.Symbol si i <$> replace t
instantiateExpr (Core.Subscript si s i t) =
  Core.Subscript si <$> instantiateExpr s
                    <*> instantiateExpr i
                    <*> replace t

instantiateExpr (Core.Subslice si s (Core.Range e1 e2) t) =
  Core.Subslice si <$> instantiateExpr s
                   <*> (Core.Range <$> instantiateExpr e1 <*> instantiateExpr e2)
                   <*> replace t
instantiateExpr (Core.Subslice si s (Core.RangeTo e) t) =
  Core.Subslice si <$> instantiateExpr s
                   <*> (Core.RangeTo <$> instantiateExpr e)
                   <*> replace t
instantiateExpr (Core.Subslice si s (Core.RangeFrom e) t) =
  Core.Subslice si <$> instantiateExpr s
                   <*> (Core.RangeFrom <$> instantiateExpr e)
                   <*> replace t

instantiateExpr (Core.UnaryOp si o e t) =
  Core.UnaryOp si o <$> instantiateExpr e <*> replace t
instantiateExpr (Core.BinaryOp si o e1 e2 t) =
  Core.BinaryOp si o <$> instantiateExpr e1
                     <*> instantiateExpr e2
                     <*> replace t
instantiateExpr (Core.Application si f p t) =
  Core.Application si <$> instantiateExpr f
                      <*> instantiateExpr p
                      <*> replace t
instantiateExpr (Core.NoArgApplication si f t) =
  Core.NoArgApplication si <$> instantiateExpr f
                           <*> replace t
instantiateExpr (Core.ForeignFnApplication si f ps t) =
  Core.ForeignFnApplication si <$> instantiateExpr f
                               <*> mapM instantiateExpr ps
                               <*> replace t
instantiateExpr (Core.Fn si a b t) =
  Core.Fn si a <$> instantiateExpr b
               <*> replace t
instantiateExpr (Core.NoArgFn si b t) =
  Core.NoArgFn si <$> instantiateExpr b
                  <*> replace t
instantiateExpr (Core.Let si name e b t) =
  Core.Let si name <$> instantiateExpr e
                   <*> instantiateExpr b
                   <*> replace t
instantiateExpr (Core.Literal si l t) =
  Core.Literal si l <$> replace t
instantiateExpr (Core.If si c tb eb t) =
  Core.If si <$> instantiateExpr c
             <*> instantiateExpr tb
             <*> instantiateExpr eb
             <*> replace t
instantiateExpr (Core.Tuple si fe se rs t) =
  Core.Tuple si <$> instantiateExpr fe
                <*> instantiateExpr se
                <*> mapM instantiateExpr rs
                <*> replace t
instantiateExpr (Core.Slice si es t) =
  Core.Slice si <$> mapM instantiateExpr es
                <*> replace t
instantiateExpr (Core.Block si es t) =
  Core.Block si <$> mapM instantiateExpr es
                <*> replace t
instantiateExpr (Core.RecordInitializer si t fields) =
  Core.RecordInitializer si <$> replace t <*> mapM instantiateField fields
instantiateExpr (Core.RecordFieldAccess si expr name t) =
  Core.RecordFieldAccess si <$> instantiateExpr expr <*> return name <*> replace t
instantiateExpr (Core.PackageMemberAccess si pkgAlias name t) =
  Core.PackageMemberAccess si pkgAlias name <$> replace t

-- | Given a polymorphically typed expression and a monomorphic type, return
-- the expression with all types substitued for monomorphic ones. If there's
-- a mismatch an error is thrown.
instantiate :: Core.Expr Poly.Type
            -> Mono.Type
            -> Either InstantiateError (Core.Expr Poly.Type)
instantiate expr mono = do
  subst <- getSubstitutions (Core.typeOf expr) mono
  (expr', _) <- runExcept (runStateT (instantiateExpr expr) subst)
  return expr'
