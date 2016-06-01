{-# LANGUAGE TupleSections #-}
module Infer
  ( analyzeExpr
  ) where

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
import qualified Data.Map                  as M
import           Data.Maybe

-- | Top level function for analyzing an Expr.
-- It outputs the type annotated Expr and its type.
analyzeExpr :: Expr () ()
            -> Maybe (Expr Pi (Ty Int), Ty Int)
analyzeExpr e =
  case runGen . runMaybeT . algorithmW M.empty $ uniquifyPPoints e of
    Nothing -> Nothing
    Just (e', t, s) -> return (second (apply s) e', t)

-- | Make program points unique.
-- Care with calling the function without specifying e.
uniquifyPPoints :: Expr b a -> Expr Pi a
uniquifyPPoints = runGen . bimapM (const gen) return

type Env a = Map Name (TyScheme a)

extend :: Name -> TyScheme a -> Env a -> Env a
extend = M.insert
type InferM a = MaybeT (Gen Int) a


-- | Main algorithm-W implementation.
-- It make use of Maybe monad to handle
-- failure and Gen monad to generate fresh
-- type variable names.
algorithmW :: Env Int
           -> Expr Int ()
           -> InferM (Expr Int (Ty Int), Ty Int, Subst Ty Int)
algorithmW env expr =
  case expr of
    Integer n -> return (Integer n, I, empty)
    Bool    b -> return (Bool b, B, empty)
    Var n     -> do
      ty <- hoistMaybe $ M.lookup n env
      i  <- lift $ instantiate ty
      return (Var n, i, empty)
    Fun pi _ f x e    -> do
      a1 <- V <$> gen
      a2 <- V <$> gen
      (e', t2,phi1) <- algorithmW (extend f (Base (Arrow a1 a2)) (extend x (Base a1) env)) e
      phi2          <- hoistMaybe $ unify t2 (apply phi1 a2)
      let t = Arrow (apply phi2 (apply phi1 a1)) (apply phi2 a2)
      return (Fun pi t f x e', t,phi2 `compose` phi1)
    Fn  pi _   n e -> do
      alpha <- V <$> gen
      (e', t2, phi)  <- algorithmW (extend n (Base alpha) env)  e
      let t = Arrow (apply phi alpha) t2
      return (Fn pi t n e', t, phi)
    App     e1 e2 -> do
      (e1', t1, phi1) <- algorithmW env e1
      (e2', t2, phi2) <- algorithmW (M.map (apply' phi1) env) e2
      alpha <- V <$> gen
      phi3  <- hoistMaybe $ unify (apply phi2 t1) (Arrow t2 alpha)
      return (App e1' e2',apply phi3 alpha, phi3 `compose` phi2 `compose` phi1)
    Let     n e1 e2  -> do
      (e1', t1, phi1) <- algorithmW env e1
      let nEnv = M.map (apply' phi1) env
      (e2', t, phi2)  <- algorithmW (extend n (generalize (foldMap toList nEnv) t1) nEnv) e2
      return (Let n e1' e2', t, phi2 `compose` phi1)
    ITE     e1 e2 e3 -> do
      (e1', t1, phi1) <- algorithmW env e1
      (e2', t2, phi2) <- algorithmW (M.map (apply' phi1) env) e2
      (e3', t3, phi3) <- algorithmW (M.map (apply' phi2 . apply' phi1) env) e3
      phi4 <- hoistMaybe $ unify ((apply phi3 . apply phi2) t1) B
      phi5 <- hoistMaybe $ unify ((apply phi4 . apply phi3) t2) (apply phi4 t3)
      return (ITE e1' e2' e3', (apply phi5 . apply phi4) t3, phi5 `compose` phi4 `compose` phi3 `compose` phi2 `compose` phi1)
    Oper    op e1 e2 -> do
      (e1', t1, phi1) <- algorithmW env e1
      (e2', t2, phi2) <- algorithmW (M.map (apply' phi1) env) e2
      phi3 <- hoistMaybe $ unify (apply phi2 t1) (tOp op)
      phi4 <- hoistMaybe $ unify (apply phi3 t2) (tOp op)
      return (Oper op e1' e2', tOp op, phi4 `compose` phi3 `compose` phi2 `compose` phi1)

tOp :: Op -> Ty a
tOp op =
  case op of
    Add -> I
    Sub -> I
    Mul -> I
    Div -> I

-- | Instantiate a type scheme with fresh variables
-- drawn from the Gen monad.
instantiate :: (Eq a, Enum a) => TyScheme a -> Gen a (Ty a)
instantiate (Base t)     = return t
instantiate (Forall s)   = do
  let bvars = L.nub $ B.bindings s
  nvars <- M.fromList <$> mapM (\x -> (x,) <$> gen) bvars
  return $ B.instantiate V (B.mapBound (fromJust . flip M.lookup nvars) s)

-- | Generalize a type by turning it into a type scheme
-- and bounding all variables not present in the list.
generalize :: Eq a => [a] -> Ty a -> TyScheme a
generalize bv t = Forall $ B.abstract (`L.elemIndex` (toList t L.\\ bv)) t
