module TypeChecker where

import Data.Bifunctor (first) -- first applies a function to the left side of an Either, leaving the right side untouched. We use it to wrap incoming ContextError as TypeError.  --  first CtxError wraps a ContextError into a TypeError, so the types line up in the do block

import Syntax
import Context
import ErrorTypes

---------------------------
-- INFER
---------------------------
infer :: Ctx -> Term -> Either TypeError Type

infer ctx (Variable x) =
  case lookupType x ctx of
    Right ty -> Right ty
    Left err -> Left $ CtxError err

infer _ UnitVal = 
  Right Unit

infer _ Zero = 
  Right Nat

infer ctx (Suc t) = do
  ty <- infer ctx t
  if ty == Nat
    then Right Nat
    else Left $ TypeMismatch { term = Suc t, expected = Nat, got = ty }

infer ctx (Pair t1 t2) = do
  ty1 <- infer ctx t1
  ty2 <- infer ctx t2
  Right (Prod ty1 ty2)

infer _ (Inj i t) = 
  Left $ MissingAnnotation (Inj i t)

infer ctx (Proj i t) = do
  ty <- infer ctx t
  case (i, ty) of
    (Fst, Prod a _) -> Right a
    (Snd, Prod _ b) -> Right b
    _               -> Left $ OtherError "Proj: expected a product type"

infer ctx (Let x t1 t2) = do
  ty1 <- infer ctx t1
  let ctx' = addVar x ty1 ctx
  infer ctx' t2

infer _ (Lam x t) = 
  Left $ MissingAnnotation (Lam x t)

infer ctx (App f t) = do
  fTy <- infer ctx f
  case fTy of
    Fun a b -> do
                _ <- check ctx t a
                Right b
    _       -> Left $ OtherError "Application expected function type"

infer ctx (Case t x1 t1 x2 t2) = do
  ty <- infer ctx t
  case ty of
    Sum a1 a2 -> do
      ty1 <- infer (addVar x1 a1 ctx) t1
      ty2 <- infer (addVar x2 a2 ctx) t2
      if ty1 == ty2
        then Right ty1
        else Left $ OtherError "Case branches have different types"
    _         -> Left $ OtherError "Case expected sum type as argument"

infer _ (Delay cl t) = 
  Left $ MissingAnnotation (Delay cl t)

infer ctx (Adv v) =
  if not $ isValue v 
    then Left $ ExpectedValue (Adv v)
    else do 
      let cl = Clock v
      ctxBeforeTick <- first CtxError $ preTickOnSpecificClock ctx cl
      tyBeforeTick <- infer ctxBeforeTick v
      case tyBeforeTick of
        EL a -> Right a
        _    -> Left $ ExpectedEL {term = Adv v, got = tyBeforeTick}
  
infer ctx (AdvAL v) =
  if not $ isValue v 
    then Left $ ExpectedValue (AdvAL v) 
    else do 
      ctxBeforeTick <- first CtxError $ preTick ctx
      ty <- infer ctxBeforeTick v
      case ty of
        AL a -> Right a
        _    -> Left $ ExpectedAL { term = AdvAL v, got = ty}

infer ctx (Select v1 v2) =
  if not (isValue v1) || not (isValue v2) 
    then Left $ ExpectedValue (Select v1 v2)
    else do 
      let clockUnion = ClockUnion (Clock v1) (Clock v2)
      ctxBeforeTick <- first CtxError $ preTickOnSpecificClock ctx clockUnion
      ty1 <- infer ctxBeforeTick v1
      ty2 <- infer ctxBeforeTick v2
      case (ty1, ty2) of
        (EL a1, EL a2) -> Right (Sum 
                                  (Sum  (Prod a1 (EL a2))
                                        (Prod (EL a1) a2))
                                  (Prod a1 a2))
        _              -> Left $ OtherError "Select: expected two delayed computations EL A1 and EL A2"

infer ctx (Always t) = do
  let stableCtx' = stableCtx ctx
  ty <- infer stableCtx' t
  Right (Box ty)

infer ctx (Unbox t) = do
  ty <- infer ctx t
  case ty of
    Box a -> Right a
    _     -> Left $ OtherError "(Unbox t) expected Box A"

infer _ (Fix r t) = 
  Left $ MissingAnnotation (Fix r t)
 
infer _ Never = 
  Left $ MissingAnnotation Never

infer _ (Wait k) = 
  if isPush k && k `elem` inputChans
    then Right (EL (chanType k))
    else Left $  ExpectedPushChannel (Wait k)

infer _ (Read k) =
  if isBuf k && k `elem` inputChans
    then Right (chanType k)
    else Left $ ExpectedBufferChannel (Read k)

infer ctx (Head t) = do
  ty <- infer ctx t
  case ty of
    Sig a -> Right a
    _     -> Left $ ExpectedSig { term = Head t, got = ty }

infer ctx (Tail t) = do
  ty <- infer ctx t
  case ty of
    Sig a -> Right (EL (Sig a))
    _     -> Left $ ExpectedSig { term = Tail t, got = ty }

infer ctx (Cons hd tl) = do
  headTy <- infer ctx hd
  _      <- check ctx tl (EL (Sig headTy))
  Right (Sig headTy)

infer ctx (Anno t ty) = do
  _ <- check ctx t ty
  Right ty

---------------------------
-- CHECK
---------------------------
check :: Ctx -> Term -> Type -> Either TypeError Type

-- Pair
check ctx (Pair t1 t2) (Prod ty1 ty2) = do
  _ <- check ctx t1 ty1
  _ <- check ctx t2 ty2
  Right (Prod ty1 ty2)

-- Injection
check ctx (Inj InLeft t) (Sum ty1 ty2) = do
  _ <- check ctx t ty1
  Right (Sum ty1 ty2)

check ctx (Inj InRight t) (Sum ty1 ty2) = do
  _ <- check ctx t ty2
  Right (Sum ty1 ty2)

-- Let
check ctx (Let x t1 t2) ty = do
  ty1 <- infer ctx t1
  _ <- check (addVar x ty1 ctx) t2 ty
  Right ty

-- Lambda
check ctx (Lam x body) (Fun argTy returnTy) = do
  _ <- check (addVar x argTy ctx) body returnTy
  Right (Fun argTy returnTy)

-- Case
check ctx (Case t x1 t1 x2 t2) ty = do
  arg <- infer ctx t
  case arg of
    Sum ty1 ty2 -> do -- check that t is a sum type
      _ <- check (addVar x1 ty1 ctx) t1 ty
      _ <- check (addVar x2 ty2 ctx) t2 ty
      Right ty
    _ -> Left (OtherError ("Case argument " ++ show t ++ "should have type Sum _ _, but got type " ++ show arg))

-- Delay
check ctx (Delay cl t) (EL ty) = do
  _ <- check (addTick cl ctx) t ty
  Right (EL ty)

-- Always/box
check ctx (Always t) (Box ty) = do
  _ <- check (stableCtx ctx) t ty
  Right (Box ty)

-- Fix
check ctx (Fix x t) ty = do
  let ctx' = addVar x (AL ty) (stableCtx ctx)
  _ <- check ctx' t ty
  Right ty

-- Never
check _ Never (EL a) =
  Right (EL a)

-- Cons
check ctx (Cons hd tl) (Sig ty) = do
  _ <- check ctx hd ty
  _ <- check ctx tl (EL (Sig ty))
  Right (Sig ty)

-- Default
check ctx t ty =
  case infer ctx t of
    Right ty' | ty == ty' -> Right ty
    Right ty'             -> Left $ TypeMismatch { term = t, expected = ty, got = ty' }
    Left err              -> Left err