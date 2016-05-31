module Infer
where

import Expr
import Type

import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Functor.Identity
import Control.Error.Util
type InferM a = MaybeT (State Char) a

type Env a = Map Name (TyScheme a)

extend :: Name -> TyScheme a -> Env a -> Env a
extend = M.insert

runInfer :: Expr -> Maybe (Ty Char, Subst Ty Char)
runInfer = (`evalState` ('a' :: Char )) . runMaybeT . algorithmW M.empty

algorithmW :: Env Char
           -> Expr
           -> InferM (Ty Char, Subst Ty Char)
algorithmW env expr =
  case expr of
    Integer _ -> return ( I, empty)
    Bool    _ -> return ( B, empty)
    Var n     -> do
      ty <- hoistMaybe $ M.lookup n env
      return (inst ['a'..] ty, empty)
    Fun pi f x e    -> do
      a1 <- V <$> get
      a2 <- V <$> get
      (t2,phi1) <- algorithmW (extend f (Base (Arrow a1 a2)) (extend x (Base a1) env)) e
      phi2      <- hoistMaybe $ unify t2 (apply phi1 a2)
      return (Arrow (apply phi2 (apply phi1 a1)) (apply phi2 a2),phi2 `compose` phi1)
    Fn      pi   n e -> do
      alpha <- V <$> get
      (t2, phi)  <- algorithmW (extend n (Base alpha) env)  e
      return (Arrow (apply phi alpha) t2, phi)
    App     e1 e2 -> do
      (t1, phi1) <- algorithmW env e1
      (t2, phi2) <- algorithmW (M.map (apply' phi1) env) e2
      alpha <- V <$> get
      phi3  <- hoistMaybe $ unify (apply phi2 t1) (Arrow t2 alpha)
      return (apply phi3 alpha, phi3 `compose` phi2 `compose` phi1)
    Let     n e1 e2  -> do
      (t1, phi1) <- algorithmW env e1
      let nEnv = M.map (apply' phi1) env
      (t, phi2)  <- algorithmW (extend n (gen' (M.keys undefined) t1) nEnv) e2
      return (t, phi2 `compose` phi1)
    ITE     e1 e2 e3 -> do
      (t1, phi1) <- algorithmW env e1
      (t2, phi2) <- algorithmW (M.map (apply' phi1) env) e2
      (t3, phi3) <- algorithmW (M.map (apply' phi2 . apply' phi1) env) e3
      phi4 <- hoistMaybe $ unify ((apply phi3 . apply phi2) t1) B
      phi5 <- hoistMaybe $ unify ((apply phi4 . apply phi3) t2) (apply phi4 t3)
      return ((apply phi5 . apply phi4) t3, phi5 `compose` phi4 `compose` phi3 `compose` phi2 `compose` phi1)
    Oper    op e1 e2 -> do
      (t1, phi1) <- algorithmW env e1
      (t2, phi2) <- algorithmW (M.map (apply' phi1) env) e2
      phi3 <- hoistMaybe $ unify (apply phi2 t1) (tOp op)
      phi4 <- hoistMaybe $ unify (apply phi3 t2) (tOp op)
      return (tOp op, phi4 `compose` phi3 `compose` phi2 `compose` phi1)

tOp :: Op -> Ty a
tOp op =
  case op of
    Add -> I
    Sub -> I
    Mul -> I
    Div -> I

