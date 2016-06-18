{-# LANGUAGE TupleSections #-}
module Infer
where

import           Expr
import           Type

import qualified Bound.Scope.Simple         as B
import           Control.Error.Util
import           Control.Monad.Gen
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Bifoldable            (bifoldMap, bifoldl)
import           Data.Bifunctor             (first)
import           Data.Bitraversable         (bimapAccumL, bimapM)
import           Data.Foldable
import           Data.List                  as L
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Set                   (Set)
import qualified Data.Set                   as Set


-- | Top level function for analyzing an Expr.
-- It outputs the type annotated Expr and its type.
analyzeExpr :: AnnF Pi ()
            -> Either (TyErr AnnVar TyVar)
                      ( AnnF Pi (Ty (Set Pi) TyVar) -- ^ Expr annotated with types and PP
                      , Ty (Set Pi) TyVar)          -- ^ Type of expr with PP
analyzeExpr e = do
  (an, t, s, c) <- runGenFrom '`' . runGenT
                 . runExceptT . algorithmW Map.empty $ e
  let sc = solveConstraints (Set.toList c)

  return ( fmap ((`replaceAnn` sc) . apply s) an
         , replaceAnn t sc)

runInfer :: InferM a -> Either (TyErr AnnVar TyVar) a
runInfer = runGenFrom '`' . runGenT
                 . runExceptT
type Pi     = Char -- Program points (we use uppercase letters begining in P)
type TyVar  = Char -- Type variables
type AnnVar = Int  -- Annotation variables

-- | Make program points unique.
uniquifyPPoints :: AnnF () a -> AnnF Pi a
uniquifyPPoints = snd . bimapAccumL (\acc _ -> (succ acc, acc)) (\a d -> (a,d)) 'P'

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
algorithmW :: Env AnnVar TyVar
           -> AnnF Pi ()
           -> InferM ( AnnF Pi (Ty Int TyVar)
                     , Ty Int TyVar
                     , Subst Int TyVar
                     , Set (Constraint Pi AnnVar))
algorithmW env (AnnF _ expr) =
  case expr of
    Integer n -> return (ann I (Integer n), I, mempty, mempty)

    Bool    b -> return (ann I (Bool b) , B, mempty, mempty)

    Var n     ->
      case Map.lookup n env of
        Just ty -> do
          i      <- lift . lift $ instantiate ty
          return (ann i (Var n) ,  i, mempty, mempty)
        Nothing ->
          throwE (TyErrInternal ("Variable lookup fail: " ++ show n))

    Fun pi f x e    -> do
      a_0 <- freshTyVar
      a_1 <- freshTyVar
      b_0 <- freshAnnVar

      (e',t1,phi1,c1) <- algorithmW (extend f (Base (TyArrow a_0 b_0 a_1)) (extend x (Base a_0) env)) e

      phi2  <- unify t1 (apply phi1 a_1)

      let t = TyArrow (apply (phi2 <> phi1) a_0) b_0 (apply phi2 t1)

      return ( ann t (Fun pi f x e')
             , t
             , phi2 <> phi1
             , (b_0 >: pi) <> c1)

    Fn  pi f e -> do
      a_0 <- freshTyVar
      (e', t1, phi, c0)  <- algorithmW (extend f (Base a_0) env)  e
      b_0 <- freshAnnVar
      let t = TyArrow (apply phi a_0) b_0 t1

      return ( ann t (Fn pi f e')
             , t
             , phi
             , (b_0 >: pi) <>  c0)

    App     e1 e2 -> do
      (e1', t1, phi1, c1) <- algorithmW env e1
      (e2', t2, phi2, c2) <- algorithmW (Map.map (apply phi1) env) e2

      a_0 <- freshTyVar
      b_0 <- freshAnnVar

      phi3 <- unify (apply phi2 t1) (TyArrow t2 b_0 a_0)

      let t = apply (phi3 <> phi2) a_0

      return ( ann t (App e1' e2')
             , t
             , phi3 <> phi2 <> phi1
             , c1 <> c2 <> apply phi3 t2 `flows` apply (phi3 <> phi2) t1)

    Let     n e1 e2  -> do
      (e1', t1, phi1, c1) <- algorithmW env e1

      let nEnv = Map.map (apply phi1) env

      (e2', t, phi2, c2)  <- algorithmW (extend n (generalize (foldMap toList nEnv) t1) nEnv) e2

      return ( ann t (Let n e1' e2')
             , t
             , phi2 <> phi1
             , c1 <> c2)

    ITE     e1 e2 e3 -> do
      (e1', t1, phi1, c1) <- algorithmW env e1
      (e2', t2, phi2, c2) <- algorithmW (Map.map (apply phi1) env) e2
      (e3', t3, phi3, c3) <- algorithmW (Map.map (apply (phi2 <> phi1)) env) e3

      phi4 <-  unify (apply (phi3 <> phi2) t1) B
      phi5 <-  unify (apply (phi4 <> phi3) t2) (apply phi4 t3)

      (c, t)  <- subeffect (apply (phi5 <> phi4 <> phi3) t2) (apply (phi5 <> phi4 ) t3)

      return ( ann t (ITE e1' e2' e3')
             , t
             , phi5 <> phi4 <> phi3 <> phi2 <> phi1
             , c1 <> c2 <> c3 <> c)

    Oper    op e1 e2 -> do
      (e1', t1, phi1, c1) <- algorithmW env e1
      (e2', t2, phi2, c2) <- algorithmW (Map.map (apply phi1) env) e2
      phi3 <-  unify (apply phi2 t1) (tOp op)
      phi4 <-  unify (apply phi3 t2) (tOp op)
      return ( ann (tOp op) (Oper op e1' e2')
             , tOp op
             , phi4 <> phi3 <> phi2 <> phi1
             , c1 <> c2)

    -- Pairs
    Pair pi e1 e2 -> do
      (e1', t1, phi1, c1) <- algorithmW env e1
      (e2', t2, phi2, c2) <- algorithmW (Map.map (apply phi1) env) e2

      b_0 <- freshAnnVar

      let t = TyPair (apply phi2 t1) b_0 t2

      return ( ann t (Pair pi e1' e2')
             , t
             , phi2 <> phi1
             , c1 <> c2 <> b_0 >: pi)

    PCase e0 x1 x2 e2 -> do
      (e0', t0, phi0, c0) <- algorithmW env e0
      a_0 <- freshTyVar
      a_1 <- freshTyVar
      b_0 <- freshAnnVar

      phi1 <- unify (apply phi0 t0) (TyPair a_0 b_0 a_1)
      (e2', t2, phi2, c2) <- algorithmW (extend x2 (apply phi1 (Base a_1))
                          $ extend x1 (apply phi1 (Base a_0)) $ Map.map (apply (phi1 <> phi0)) env) e2

      return ( ann t2 (PCase e0' x1 x2 e2')
             , t2
             , phi2 <> phi1
             , c0 <> c2)

      -- Lists
    LNil pi -> do
        a_0 <- freshTyVar
        b_0 <- freshAnnVar

        let t = TyList b_0 a_0

        return ( ann t (LNil pi)
               , t
               , mempty
               , b_0 >: pi)

    LCons pi e0 e1 -> do
        (e0', t0, phi0, c0) <- algorithmW env e0
        (e1', t1, phi1, c1) <- algorithmW (Map.map (apply phi0) env) e1

        b_0 <- freshAnnVar

        phi2  <- unify t1 (TyList b_0 (apply (phi1 <> phi0) t0))

        (c,t) <- subeffect (apply (phi2 <> phi1) t1) (TyList b_0 (apply (phi2 <> phi1 <> phi0) t0))

        return ( ann t (LCons pi e0' e1')
               , t
               , phi2 <> phi1 <> phi0
               , c0 <> c1 <> (b_0 >: pi) <> c )

    LCase e0 hd tl e1 e2 -> do
      (e0', t0, phi0, c0) <- algorithmW env e0

      a_0 <- freshTyVar

      b_0 <- freshAnnVar

      phi1 <- unify t0 (TyList b_0 a_0)


      (e1', t1, phi2, c1) <- algorithmW (extend hd (apply phi1 (Base a_0))
                           $ extend tl (apply phi1 (Base t0)) $ Map.map (apply (phi1 <> phi0)) env) e1

      (e2', t2, phi3, c2) <- algorithmW (extend tl (apply (phi2 <> phi1) (Base t0)) $ Map.map (apply (phi2 <> phi1 <> phi0)) env) e2

      phi4 <- unify (apply (phi3 <> phi2) t1) (apply phi3 t2)

      (c,t) <- subeffect (apply (phi4 <> phi3 <> phi2) t1) (apply (phi4 <> phi3) t2)

      return ( ann t (LCase e0' hd tl e1' e2')
             , t
             , phi4 <> phi3 <> phi2 <> phi1 <> phi0
             , c0 <> c1 <> c2 <> c)



-- | Type unification.
-- Unification only works on types as originally defined.
-- It does not deal in any case with annotation variables.
unify :: (Ord a, Monad m)
      => Ty b a
      -> Ty b a
      -> ExceptT (TyErr b a) m (Subst b a)
unify I I   = return  mempty
unify B B   = return  mempty
unify (TyV f1) (TyV f2)
    | f1 == f2  = return mempty
    | otherwise = return (f1 ~> TyV f2)
unify (TyArrow t1 a t2) (TyArrow t1' a' t2') = do
  phi0  <- unify t1 t1'
  phi1  <- unify (apply phi0 t2) (apply phi0 t2')
  return ( phi1 <> phi0)
unify (TyV f1) t
  | occursCheck f1 t = throwE (TyErrOccursCheck f1 t)
  | otherwise        =
    return (f1 ~> t)
unify t (TyV f1)
  | occursCheck f1 t = throwE (TyErrOccursCheck f1 t)
  | otherwise        =
    return (f1 ~> t)
unify (TyPair t1 _ t2) (TyPair t1' _ t2') = do
  phi0 <- unify t1 t1'
  phi1 <- unify (apply phi0 t2) (apply phi0 t2')
  return (phi1 <> phi0)
unify (TyList _ t1) (TyList _ t1') = unify t1 t1'
unify t k = throwE (TyErrUnify t k)

-- | Occurs check
occursCheck :: Eq a => a -> Ty b a -> Bool
occursCheck = elem

tOp :: Op -> Ty b a
tOp op =
  case op of
    Add -> I
    Sub -> I
    Mul -> I
    Div -> I

-- | Generate a constraint indicating that t1 can flow to the
-- argument of the function type t2. It is important that both
-- types are equal before proceding.
flows :: (Ord b, Ord c)
      => Ty b a
      -> Ty b a
      -> Set (Constraint c b)
flows t1 (TyArrow t _ _) =
  let ann1 = bifoldMap (return :: a -> [a]) (const mempty) t1
  in fst $ bifoldl
        (\(acc,a1:as) a2
          -> (acc <> Set.singleton (C a2 (AnnV a1)), as))
        const
        (mempty, ann1) t

-- | Given two equal (important) types t1 and t2, create a new type t
-- with all fresh annotation variables and return also the constraints
-- between fresh variables and the old annotation variables.
subeffect :: Ty AnnVar a
          -> Ty AnnVar a
          -> InferM (Set (Constraint Pi AnnVar), Ty AnnVar a)
subeffect t1 t2 =
  let ann1 = bifoldMap (return :: a -> [a]) (const mempty) t1
  in (first fst . bimapAccumL
        (\(acc,a1:as) (a2,b)
          -> ((acc <> Set.singleton (C b (AnnV a2))
                   <> Set.singleton (C b (AnnV a1)), as), b))
    (,)
    (mempty, ann1))
     <$> bimapM (\ann -> (ann,) <$> freshAnnVar) return t2

-- | Solve the set of generated constraints.
--   return a map from annotation variables to sets of pp.
solveConstraints :: [Constraint Pi AnnVar] -> Map AnnVar (Set Pi)
solveConstraints cs =
      -- initial map with all variable map to empty set
  let init = Map.fromList . map (,Set.empty) . nub
           . concatMap toList $ cs

      -- recursive workhorse function.
      go m []     = m
      go m (C b (AnnV a):xs) =
        let (Just sb, Just sa) = (Map.lookup b m, Map.lookup a m)
        in if sa `Set.isSubsetOf` sb
            then go m xs
            else go (Map.insertWith Set.union b sa m) (xs ++ involved b)

      go m (C b (AnnS a):xs) =
        let Just s = Map.lookup b m
        in if a `Set.isSubsetOf` s
            then go m xs
            else go (Map.insertWith Set.union b a m) (xs ++ involved b)

      involved b =
        filter (\c@(C v a) -> v == b || case a of
                                          AnnV a' -> a' == b
                                          _ -> False ) cs

  in go init cs

replaceAnn :: Ty AnnVar a
           -> Map AnnVar (Set Pi)
           -> Ty (Set Pi) a
replaceAnn t m =
  first (\v -> fromMaybe Set.empty (Map.lookup v m)) t

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
