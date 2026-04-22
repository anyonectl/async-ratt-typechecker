module TypeChecker where

import Data.Bifunctor (first) -- first applies a function to the left side of an Either, leaving the right side untouched. We use it to wrap incoming ContextError as TypeError

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
    _               -> Left $ TypeMismatch { term = Proj i t, expected = Prod a b, got = ty} -- not sure this error works here.. 

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
      check ctx t a
      Right b
    _ -> Left (OtherError "Application expected function type")

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
    then Left $ ExpectedValue { term = Adv v, got = v }
    else do
      ctxBeforeTick <- first CtxError $ preTick ctx --  first CtxError wraps a ContextError into a TypeError, so the types line up in the do block
      ty <- infer ctxBeforeTick v
      case ty of
        EL a -> Right a
        _    -> Left $ ExpectedEL {term = Adv v, got = ty}

-- AdvAL
infer ctx (AdvAL v) =
  if not $ isValue v
    then Left $ ExpectedValue { term = AdvAL v, got = v }
    else do
      ctxBeforeTick <- first CtxError $ preTick ctx
      ty <- infer ctxBeforeTick v
      case ty of
        AL a -> Right a
        _    -> Left $ ExpectedAL { term = AdvAL v, got = ty}

-- Select
infer ctx (Select v1 v2) =
  if not (isValue v1) || not (isValue v2)
    then Left $ ExpectedTwoValues { term = Select v1 v2, got1 = v1, got2 = v2}
    else do
      ty1 <- infer ctx v1 -- premise 1: v_1 : EL a_1
      ty2 <- infer ctx v2 -- premise 2: v_2 : EL a_2
      -- premise 3: the clock is a union of cl(v_1) and cl(v_2)
      -- how do we check premise 3? 
      ctxBeforeTick <- first CtxError $ preTick ctx -- premise 4: there is a tick free context
      case (ty1, ty2) of
        (EL a1, EL a2) -> Right (Sum (Sum (Prod a1 (EL a2))
                                          (Prod (EL a1) a2))
                                (Prod a1 a2))
        _ -> Left (OtherError "Select expected two delayed computations of the same type")

-- Always
-- Unbox

-- Wait (17)
infer ctx (Wait k) = case k of
  Push a    -> Right EL a
  PushBuf a -> Right EL a
  _         -> Left $  ExpectedPushChannel (Wait k)

-- Read (18)
infer ctx (Read k) = case k of
  Buf a     -> Right a
  PushBuf a -> Right a
  _         -> Left $ ExpectedBufferChannel (Read k)

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
  check ctx tl (EL (Sig headTy))
  return (Sig headTy) -- need to understand better what return does here

-- Switch from infer mode to check mode (when do we need this?)
infer ctx (Anno t ty) = check ctx t ty

-- Default
infer _ _ = Left (OtherError "Infer failed..")


---------------------------
-- CHECK
---------------------------
check :: Ctx -> Term -> Type -> Either TypeError Type

-- Pair
check ctx (Pair t1 t2) ty =
  case ty of
    Prod a b -> do
      check ctx t1 a
      check ctx t2 b
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
      check ctx1 t1 ty
      check ctx2 t2 ty -- the error checking is better when using infer..? 
      Right ty                    -- then for each extended context branch, check that the type of t1 and t2 are ty
    _ -> Left (OtherError "Case argument is not a sum type")

-- Delay
check ctx (Delay cl t) (EL ty) = do
  check (addTick cl ctx) t ty
  return (EL ty) -- need to understand better what return does here

check _ (Delay _ _) expected =
    Left (OtherError
        ("not a delayed type: " ++ show expected))

-- Always
-- Unbox

check _ Never ty = 
  case ty of 
    EL a  -> Right (EL a)
    _     -> Left $ ExpectedEL { term = Never, got = ty }

-- Fix
check ctx (Fix x t) ty = do
  let ctx' = addVar x (AL ty) (stableCtx ctx)
  check ctx' t ty

-- Cons
check ctx (Cons hd tl) (Sig ty) = do
  check ctx hd ty
  check ctx tl (EL (Sig ty))
  return (Sig ty) -- need to understand better what return does here

-- Default
check ctx t ty =
  case infer ctx t of
    Right ty' | ty == ty' -> Right ty
    Right ty'             -> Left (TypeMismatch { expected = ty, got = ty' })
    Left err              -> Left err