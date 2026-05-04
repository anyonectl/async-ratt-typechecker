module TypeChecker where

import Data.Bifunctor (first) -- first applies a function to the left side of an Either, leaving the right side untouched. We use it to wrap incoming ContextError as TypeError.  --  first CtxError wraps a ContextError into a TypeError, so the types line up in the do block

import Syntax
import Context
import ErrorTypes

---------------------------
-- INFER
---------------------------
infer :: Ctx -> Term -> Either TypeError Type

-- Variable
infer ctx (Variable x) =
  case lookupType x ctx of
    Right ty -> Right ty
    Left err -> Left $ CtxError err

-- UnitVal
infer _ UnitVal = Right Unit

-- Zero
infer _ Zero = Right Nat

-- Succsessor
infer ctx (Suc t) = do
  ty <- infer ctx t
  if ty == Nat
    then Right Nat
    else Left $ TypeMismatch { term = Suc t, expected = Nat, got = ty }

-- Projection
infer ctx (Proj i t) = do
  ty <- infer ctx t
  case (i, ty) of
    (Fst, Prod a _) -> Right a
    (Snd, Prod _ b) -> Right b
    _               -> Left $ OtherError "Proj: expected a product type"

-- Let
infer ctx (Let x t1 t2) = do
  ty1 <- infer ctx t1
  let ctx' = addVar x ty1 ctx
  infer ctx' t2

-- Application
infer ctx (App f t) = do
  fTy <- infer ctx f
  case fTy of
    Fun a b -> do
                _ <- check ctx t a
                Right b
    _       -> Left (OtherError "Application expected function type")

-- Case
infer ctx (Case t x1 t1 x2 t2) = do
  ty <- infer ctx t
  case ty of
    Sum a1 a2 -> do
      let ctx1 = addVar x1 a1 ctx
      let ctx2 = addVar x2 a2 ctx
      ty1 <- infer ctx1 t1
      ty2 <- infer ctx2 t2
      if ty1 == ty2
        then Right ty1
        else Left (OtherError "Case branches have different types")
    _ -> Left (OtherError "Case expected sum type as argument")

-- Adv
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
  
-- AdvAL 
infer ctx (AdvAL v) =
  if not $ isValue v 
    then Left $ ExpectedValue (AdvAL v) 
    else do 
      ctxBeforeTick <- first CtxError $ preTick ctx
      ty <- infer ctxBeforeTick v
      case ty of
        AL a -> Right a
        _    -> Left $ ExpectedAL { term = AdvAL v, got = ty}

-- Select
infer ctx (Select v1 v2) =
  if not (isValue v1) && not (isValue v2) 
    then Left $ ExpectedValue (Select v1 v2)
    else do 
      let clockUnion = ClockUnion (Clock v1) (Clock v2)
      ctxBeforeTick <- first CtxError $ preTickOnSpecificClock ctx clockUnion
      ty1 <- infer ctxBeforeTick v1
      ty2 <- infer ctxBeforeTick v2
      case (ty1, ty2) of
        (EL a1, EL a2) -> Right (Sum (Sum (Prod a1 (EL a2))
                                          (Prod (EL a1) a2))
                                (Prod a1 a2))
        _              -> Left $ OtherError "Select: expected two delayed computations EL A1 and EL A2"

-- Always
infer ctx (Always t) = do
  let stableCtx' = stableCtx ctx
  ty <- infer stableCtx' t
  Right (Box ty)

-- Unbox
infer ctx (Unbox t) = do
  ty <- infer ctx t
  case ty of
    Box a -> Right a
    _     -> Left $ OtherError "(Unbox t) expected Box A"
    
-- Wait
infer _ (Wait k) = 
  if isPush k && k `elem` inputChans
    then Right (EL (chanType k))
    else Left $  ExpectedPushChannel (Wait k)

-- Read 
infer _ (Read k) =
  if isBuf k && k `elem` inputChans
    then Right (chanType k)
    else Left $ ExpectedBufferChannel (Read k)

-- Head
infer ctx (Head t) = do
  ty <- infer ctx t
  case ty of
    Sig a -> Right a
    _     -> Left (OtherError "Head expected a signal")

-- Tail
infer ctx (Tail t) = do
  ty <- infer ctx t
  case ty of
    Sig a -> Right (EL (Sig a))
    _     -> Left (OtherError "Tail expected a signal")

-- Cons
infer ctx (Cons hd tl) = do
  headTy <- infer ctx hd
  _      <- check ctx tl (EL (Sig headTy))
  Right (Sig headTy)

-- Fix
infer _ (Fix _ _) = Left $ OtherError "Cannot infer type of fix, annotate with expected type"

-- Switch from infer mode to check mode (when do we need this?)
infer ctx (Anno t ty) = do
  _ <- check ctx t ty
  return ty

-- Default
infer _ t = Left $ OtherError ("No infer rule for term: " ++ show t)


---------------------------
-- CHECK
---------------------------
check :: Ctx -> Term -> Type -> Either TypeError Type

-- Pair
check ctx (Pair t1 t2) ty =
  case ty of
    Prod a b -> do
      _ <- check ctx t1 a
      _ <- check ctx t2 b
      Right ty
    _       -> Left (OtherError "Pair expected a product type")

-- Injection
check ctx (Inj rl t) ty =
  case ty of
    Sum a b -> case rl of
      InLeft  -> check ctx t a
      InRight -> check ctx t b
    _         -> Left (OtherError "Injection expected sum type")

-- Let
check ctx (Let x t1 t2) ty = do -- this is just like the Sestoft book (yay!)
  ty1 <- infer ctx t1
  let ctx' = addVar x ty1 ctx
  check ctx' t2 ty

-- Lambda
check ctx (Lam x t) ty =
  case ty of
    Fun a b -> check (addVar x a ctx) t b
    _       -> Left (OtherError "Lambda expected function type")

-- Case
check ctx (Case t x1 t1 x2 t2) ty = do
  arg <- infer ctx t
  case arg of
    Sum a1 a2 -> do               -- check that t is a sum type
      let ctx1 = addVar x1 a1 ctx
      let ctx2 = addVar x2 a2 ctx
      _ <- check ctx1 t1 ty
      _ <- check ctx2 t2 ty -- the error checking is better when using infer..? 
      Right ty                    -- then for each extended context branch, check that the type of t1 and t2 are ty
    _ -> Left (OtherError "Case argument is not a sum type")

-- Delay
check ctx (Delay cl t) (EL ty) = do
  _ <- check (addTick cl ctx) t ty
  Right (EL ty)
check _ (Delay _cl _t) _ty = Left $ ExpectedEL { term = Delay _cl _t, got = _ty }


check _ Never (EL a) = Right (EL a)
check _ Never ty      = Left $ ExpectedEL { term = Never, got = ty }

-- Fix
check ctx (Fix x t) ty = do
  let ctx' = addVar x (AL ty) (stableCtx ctx)
  check ctx' t ty

-- Cons
check ctx (Cons hd tl) (Sig ty) = do
  _ <- check ctx hd ty
  _ <- check ctx tl (EL (Sig ty))
  Right (Sig ty)

-- Always/box
check ctx (Always t) (Box ty) = do
  _ <- check (stableCtx ctx) t ty
  return (Box ty)

-- Default
check ctx t ty =
  case infer ctx t of
    Right ty' | ty == ty' -> Right ty
    Right ty'             -> Left (TypeMismatch { term = t, expected = ty, got = ty' })
    Left err              -> Left err

