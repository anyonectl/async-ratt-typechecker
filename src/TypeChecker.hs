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
    else Left $ IncompatibleType { term = Suc t, expected =  ENat, got = ty}

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
    _               -> Left $ IncompatibleType { term = Proj i t, expected = EProd, got = ty }

infer ctx (Let x t1 t2) = do
  ty1 <- infer ctx t1
  let ctx' = addVar x ty1 ctx
  infer ctx' t2

infer _ (Lam x t) =
  Left $ MissingAnnotation (Lam x t)

infer ctx (App f t) = do
  ty <- infer ctx f
  case ty of
    Fun a b -> do
                _ <- check ctx t a
                Right b
    _       -> Left $ IncompatibleType { term = App f t, expected = EFun, got = ty}

infer ctx (Case t x1 t1 x2 t2) = do
  ty <- infer ctx t
  case ty of
    Sum a1 a2 -> do
      ty1 <- infer (addVar x1 a1 ctx) t1
      ty2 <- infer (addVar x2 a2 ctx) t2
      if ty1 == ty2
        then Right ty1
        else Left $ OtherError ("Mismatch in term: " ++ show (Case t x1 t1 x2 t2) ++ ", branches have different types")
    _         -> Left $ IncompatibleType { term = t, expected = ESum, got = ty}

infer ctx (Delay cl t) = do
  ty <- infer (addTick cl ctx) t
  Right (EL ty)

infer ctx (Adv v) =
  if not $ isValue v
    then Left $ IncompatibleTerm { term = Adv v, expected = EValue, gotT = v}
    else do
      let cl = Clock v
      ctxBeforeTick <- first CtxError $ preTickOnSpecificClock ctx cl
      tyBeforeTick <- infer ctxBeforeTick v
      case tyBeforeTick of
        EL a -> Right a
        _    -> Left $ IncompatibleType { term = Adv v, expected = EEL, got = tyBeforeTick }

infer ctx (AdvAL v) =
  if not $ isValue v
    then Left $ IncompatibleTerm { term = AdvAL v, expected = EValue, gotT = v }
    else do
      ctxBeforeTick <- first CtxError $ preTick ctx
      ty <- infer ctxBeforeTick v
      case ty of
        AL a -> Right a
        _    -> Left $ IncompatibleType { term = AdvAL v, expected = EAL, got = ty }

infer ctx (Select v1 v2)
  | not (isValue v1) = Left $ IncompatibleTerm { term = Select v1 v2, expected = EValue, gotT = v1 }
  | not (isValue v2) = Left $ IncompatibleTerm { term = Select v1 v2, expected = EValue, gotT = v2 }
  | otherwise = do
          let clockUnion = ClockUnion (Clock v1) (Clock v2)
          ctxBeforeTick <- first CtxError $ preTickOnSpecificClock ctx clockUnion
          ty1 <- infer ctxBeforeTick v1
          ty2 <- infer ctxBeforeTick v2
          case (ty1, ty2) of
            (EL a1, EL a2) -> Right (Sum
                                      (Sum  (Prod a1 (EL a2))
                                            (Prod (EL a1) a2))
                                      (Prod a1 a2))
            (EL _, _)     -> Left $ IncompatibleType { term = v1, expected = EEL, got = ty1 }
            (_, EL _)     -> Left $ IncompatibleType { term = v2, expected = EEL, got = ty2 }
            _             -> Left $ OtherError "Select: expected two delayed computations EL A1 and EL A2"

infer ctx (Always t) = do
  let stableCtx' = stableCtx ctx
  ty <- infer stableCtx' t
  Right (Box ty)

infer ctx (Unbox t) = do
  ty <- infer ctx t
  case ty of
    Box a -> Right a
    _     -> Left $ IncompatibleType { term = Unbox t, expected = EBox, got = ty }

infer _ (Fix r t) =
  Left $ MissingAnnotation (Fix r t)

infer _ Never =
  Left $ MissingAnnotation Never

infer _ (Wait k) =
  if isPush k && k `elem` inputChans
    then Right (EL (chanType k))
    else Left $ IncompatibleChan { term = Wait k, expected = EPushChan, gotC = k }

infer _ (Read k) =
  if isBuf k && k `elem` inputChans
    then Right (chanType k)
    else Left $ IncompatibleChan { term = Wait k, expected = EBufChan, gotC = k }

infer ctx (Head t) = do
  ty <- infer ctx t
  case ty of
    Sig a -> Right a
    _     -> Left $ IncompatibleType { term = Head t, expected = ESig, got = ty }

infer ctx (Tail t) = do
  ty <- infer ctx t
  case ty of
    Sig a -> Right (EL (Sig a))
    _     -> Left $ IncompatibleType { term = Tail t, expected = ESig, got = ty }

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

check ctx (Pair t1 t2) (Prod ty1 ty2) = do
  _ <- check ctx t1 ty1
  _ <- check ctx t2 ty2
  Right (Prod ty1 ty2)

check _   (Pair t1 t2) ty =
  Left (IncompatibleType { term = Pair t1 t2, expected = EProd, got = ty })

check ctx (Inj InLeft t) (Sum ty1 ty2) = do
  _ <- check ctx t ty1
  Right (Sum ty1 ty2)

check ctx (Inj InRight t) (Sum ty1 ty2) = do
  _ <- check ctx t ty2
  Right (Sum ty1 ty2)

check _   (Inj i t) ty =
  Left (IncompatibleType { term = Inj i t, expected = ESum, got = ty })

check ctx (Let x t1 t2) ty = do
  ty1 <- infer ctx t1
  _ <- check (addVar x ty1 ctx) t2 ty
  Right ty

check ctx (Lam x body) (Fun argTy returnTy) = do
  _ <- check (addVar x argTy ctx) body returnTy
  Right (Fun argTy returnTy)

check _   (Lam x body) ty =
  Left (IncompatibleType { term = Lam x body, expected = EFun, got = ty })

check ctx (Case t x1 t1 x2 t2) ty = do
  tTy <- infer ctx t
  case tTy of
    Sum ty1 ty2 -> do
      _ <- check (addVar x1 ty1 ctx) t1 ty
      _ <- check (addVar x2 ty2 ctx) t2 ty
      Right ty
    _ -> Left $ IncompatibleType { term = t, expected = ESum, got = tTy}

check ctx (Delay cl t) (EL ty) = do
  _ <- check (addTick cl ctx) t ty
  Right (EL ty)

check _   (Delay cl t) ty =
  Left (IncompatibleType { term = Delay cl t, expected = EEL, got = ty })

check ctx (Always t) (Box ty) = do
  _ <- check (stableCtx ctx) t ty
  Right (Box ty)

check _   (Always t) ty =
  Left (IncompatibleType { term = Always t, expected = EBox, got = ty })

check ctx (Fix x t) ty = do
  let ctx' = addVar x (AL ty) (stableCtx ctx)
  _ <- check ctx' t ty
  Right ty

check _   Never (EL ty) =
  Right (EL ty)

check _   Never ty =
  Left (IncompatibleType { term = Never, expected = EEL, got = ty })

check ctx (Cons hd tl) (Sig ty) = do
  _ <- check ctx hd ty
  _ <- check ctx tl (EL (Sig ty))
  Right (Sig ty)

check _   (Cons hd tl) ty =
  Left (IncompatibleType { term = Cons hd tl, expected = ESig, got = ty })

-- Default
check ctx t ty =
  case infer ctx t of
    Right ty' | ty == ty' -> Right ty
    Right ty'             -> Left $ TypeCheckFailed { term = t, checked = ty, inferred = ty' }
    Left err              -> Left err