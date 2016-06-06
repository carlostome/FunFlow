{-# LANGUAGE TupleSections #-}
module Infer
where

import           Expr
import           Type

import qualified Bound.Scope.Simple        as B
import           Control.Error.Util
import           Control.Monad.Gen
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Bifunctor            (second)
import           Data.Bitraversable        (bimapM)
import           Data.Foldable
import           Data.Functor.Identity
import           Data.List                 as L
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Void
import Control.Monad.Trans.Except

-- | Top level function for analyzing an Expr.
-- It outputs the type annotated Expr and its type.
analyzeExpr :: Expr Pi
            -> Either (TyErr AnnVar TyVar) (Ty AnnVar TyVar, Subst AnnVar TyVar ,Set (Constraint Pi AnnVar))
analyzeExpr =
  runGenFrom '`' . runGenT
  . runExceptT . algorithmW Map.empty

type TyVar  = Char
type AnnVar = Int

-- | Make program points unique.
uniquifyPPoints :: Expr () -> Expr Pi
uniquifyPPoints = runGen . traverse (const gen)

type Env b a = Map Name (TyScheme b a)

extend :: Name -> TyScheme b a -> Env b a -> Env b a
extend = Map.insert

type InferM a = ExceptT (TyErr AnnVar TyVar)
                        (GenT AnnVar (Gen TyVar)) a

freshTyVar :: InferM (Ty b Char)
freshTyVar = lift . lift $ TyV <$> gen

freshAnnVar :: InferM Int
freshAnnVar = gen

-- | Main algorithm-W implementation.
-- It make use of Maybe monad to handle
-- failure and Gen monad to generate fresh
-- type variable names.
algorithmW :: Env Int TyVar
           -> Expr Pi
           -> InferM (Ty Int TyVar, Subst Int TyVar, Set (Constraint Int Int))
algorithmW env expr =
  case expr of
    Integer n -> return (I, empty, mempty)

    Bool    b -> return (B, empty, mempty)

    Var n     ->
      case Map.lookup n env of
        Just ty -> do
          i  <- lift . lift $ instantiate ty
          return (i, empty, mempty)
        Nothing -> throwE (TyErrInternal ("Variable lookup fail: " ++ show n))

    Fun pi f x e    -> do
      a_1 <- freshTyVar
      a_2 <- freshTyVar
      b   <- freshAnnVar
      (t2,phi1,c0) <- algorithmW (extend f (Base (Arrow a_1 b a_2)) (extend x (Base a_1) env)) e
      (phi2,c2)    <- unify t2 (apply phi1 a_2)
      let t = Arrow (apply (phi2 `o` phi1) a_1) b (apply phi2 a_2)
      return (t, phi2 `o` phi1, c0 <> c2)

    Fn  pi n e -> do
      a <- freshTyVar
      b <- freshAnnVar
      (t2, phi, c0)  <- algorithmW (extend n (Base a) env)  e
      return ( Arrow (apply phi a) b t2
             , phi
             , Set.singleton (EqC b (PP pi)) <>  c0)

    App     e1 e2 -> do
      (t1, phi1, c1) <- algorithmW env e1
      (t2, phi2, c2) <- algorithmW (Map.map (apply phi1) env) e2
      a <- freshTyVar
      b <- freshAnnVar
      (phi3, c3)  <- unify (apply phi2 t1) (Arrow t2 b a)
      return ( apply phi3 a
             , phi3 `o` phi2 `o` phi1
             , c1 <> c2 <> c3)

    Let     n e1 e2  -> do
      (t1, phi1, c1) <- algorithmW env e1
      let nEnv = Map.map (apply phi1) env
      (t, phi2, c2)  <- algorithmW (extend n (generalize (foldMap toList nEnv) t1) nEnv) e2
      return (t, phi2 `o` phi1, c1 <> c2)

    ITE     e1 e2 e3 -> do
      (t1, phi1, c1) <- algorithmW env e1
      (t2, phi2, c2) <- algorithmW (Map.map (apply phi1) env) e2
      (t3, phi3, c3) <- algorithmW (Map.map (apply phi2 . apply phi1) env) e3
      (phi4, c4) <-  unify ((apply phi3 . apply phi2) t1) B
      (phi5, c5) <-  unify ((apply phi4 . apply phi3) t2) (apply phi4 t3)
      return (( apply phi5 . apply phi4) t3
             , phi5 `o` phi4 `o` phi3 `o` phi2 `o` phi1
             , c1 <> c2 <> c3 <> c4 <> c5)

    Oper    op e1 e2 -> do
      (t1, phi1, c1) <- algorithmW env e1
      (t2, phi2, c2) <- algorithmW (Map.map (apply phi1) env) e2
      (phi3, c3) <-  unify (apply phi2 t1) (tOp op)
      (phi4, c4) <-  unify (apply phi3 t2) (tOp op)
      return ( tOp op
             , phi4 `o` phi3 `o` phi2 `o` phi1
             , c1 <> c2 <> c3 <> c4)

    -- Pairs
    Pair pi e1 e2 -> do
      (t1, phi1, c1) <- algorithmW env e1
      (t2, phi2, c2) <- algorithmW (Map.map (apply phi1) env) e2
      b <- freshAnnVar
      return ( Prod (apply phi2 t1) b t2
             , phi2 `o` phi1
             , c1 <> c2 <> Set.singleton (EqC b (PP pi)))

    PCase e0 x1 x2 e1 -> do
      (t0, phi0, c0) <- algorithmW env e0
      a_0 <- freshTyVar
      a_1 <- freshTyVar
      b   <- freshAnnVar
      (phi1, c1) <- unify t0 (Prod a_0 b a_1)
      (t2, phi2, c2) <- algorithmW (extend x2 (apply phi1 (Base a_1))
                        $ extend x1 (apply phi1 (Base a_0)) $ Map.map (apply phi1) env) e1
      return ( t2
             , phi2 `o` phi1
             , c0 <> c1 <> c2)

-- | Type unification.
unify :: (Ord a, Ord b, Monad m)
      => Ty b a
      -> Ty b a
      -> ExceptT (TyErr b a) m (Subst b a, Set (Constraint b b))
unify I I   = return (Map.empty, mempty)
unify B B   = return (Map.empty, mempty)
unify (TyV f1) (TyV f2)
    | f1 == f2  = return (empty, mempty)
    | otherwise = return (Map.singleton f1 (TyV f2), mempty)
unify (Arrow t1 a t2) (Arrow t1' a' t2') = do
  (phi0, c0) <- unify t1 t1'
  (phi1, c1) <- unify (apply phi0 t2)
                      (apply phi0 t2')
  return (phi1 `o` phi0, c0 <> c1 <> Set.singleton (EqC a (AnnV a')))
unify (TyV f1) t
  | occursCheck f1 t = throwE (TyErrOccursCheck f1 t)
  | otherwise        = return (Map.singleton f1 t, mempty)
unify t (TyV f1)
  | occursCheck f1 t = throwE (TyErrOccursCheck f1 t)
  | otherwise        = return (Map.singleton f1 t, mempty)
unify (Prod t1 a t2) (Prod t1' a' t2') = do
  (phi0, c0) <- unify t1 t1'
  (phi1, c1) <- unify (apply phi0 t2)
                      (apply phi0 t2')
  return (phi1 `o` phi0, c0 <> c1 <> Set.singleton (EqC a (AnnV a')))
unify t k = throwE (TyErrUnify t k)

-- | Occurs check
occursCheck :: Eq a => a -> Ty b a -> Bool
occursCheck a = getAny . foldMap (Any . (==a))


tOp :: Op -> Ty b a
tOp op =
  case op of
    Add -> I
    Sub -> I
    Mul -> I
    Div -> I

{- solveConstraints :: (Show b, Ord b) => [C b b] -> Map b (Ann a b) -}
{- solveConstraints = go M.empty -}
{-   where -}
{-     go m [] = m -}
{-     go m (c@(b :> AnnV p : xs)) = -}
{-       case (M.lookup b m, M.lookup p m) of -}
{-         (Nothing,Nothing) -> go m (xs ++ c) -}
{-         (Just x,Nothing)  -> go (M.insert p x m) xs -}
{-         (Nothing,Just x)  -> go (M.insert b x m) xs -}
{-         (Just x,Just y)   -> go (M.insert b (Union y x) m) xs -}
{-     go m (b :> PP p : xs) = go (M.insertWith Union b (PP p) m) xs -}


-- | Instantiate a type scheme with fresh variables
-- drawn from the Gen monad.
instantiate :: (Eq a, Enum a) => TyScheme b a -> Gen a (Ty b a)
instantiate (Base t)     = return t
instantiate (Forall s)   = do
  let bvars = L.nub $ B.bindings s
  nvars <- Map.fromList <$> mapM (\x -> (x,) <$> gen) bvars
  return $ B.instantiate TyV (B.mapBound (fromJust . flip Map.lookup nvars) s)

-- | Generalize a type by turning it into a type scheme
-- and bounding all variables not present in the list.
generalize :: Eq a => [a] -> Ty b a -> TyScheme b a
generalize bv t = Forall $ B.abstract (`L.elemIndex` (toList t L.\\ bv)) t
