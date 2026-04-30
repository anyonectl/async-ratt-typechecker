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
{- 
infer on adv:
it is not enough to cover variables (despite what I said at last meeting)
also needs to consider wait_kappa
same goes for select (and for corresponding cases of check ?
Only use the check for variable or wait_kappa to decide whether argument t is a value or not. Then just
check/infer type of t as described by typing rule
-}
infer ctx (Adv (Variable x)) = do
  let cl = Clock (Variable x)
  ctxBeforeTick <- first CtxError $ preTickOnSpecificClock ctx cl
  tyBeforeTick <- first CtxError $ lookupType x ctxBeforeTick
  case tyBeforeTick of
    EL a -> Right a
    _    -> Left $ ExpectedEL {term = Adv (Variable x), got = tyBeforeTick}

infer ctx (Adv (Wait chan)) = do
  let cl = Clock (Wait chan)
  ctxBeforeTick <- first CtxError $ preTickOnSpecificClock ctx cl
  tyBeforeTick  <- infer ctxBeforeTick (Wait chan)
  case tyBeforeTick of
    EL a -> Right a
    _    -> Left $ ExpectedEL { term = Adv (Wait chan), got = tyBeforeTick }
  
infer _ctx (Adv v) = Left $ OtherError ("(Adv v) expected variable or wait_κ as argument, but got: " ++ show v)
-- AdvAL 
infer ctx (AdvAL (Variable x)) = do -- check that v is a value (needs to be a variable)
  ctxBeforeTick <- first CtxError $ preTick ctx
  ty <- infer ctxBeforeTick (Variable x)
  case ty of
    AL a -> Right a
    _    -> Left $ ExpectedAL { term = AdvAL (Variable x), got = ty}
infer _ctx (AdvAL _x) = Left $ OtherError ("(AdvAL x) expected variable as argument, but got: " ++ show _x)

-- Select
infer ctx (Select (Variable x1) (Variable x2)) = do
  let clockUnion = ClockUnion (Clock (Variable x1)) (Clock (Variable x2))
  ctxBeforeTick <- first CtxError $ preTickOnSpecificClock ctx clockUnion
  ty1 <- infer ctxBeforeTick (Variable x1)
  ty2 <- infer ctxBeforeTick (Variable x2)
  case (ty1, ty2) of
    (EL a1, EL a2) -> Right (Sum (Sum (Prod a1 (EL a2))
                                      (Prod (EL a1) a2))
                            (Prod a1 a2))
    _              -> Left $ OtherError "Select: expected two delayed computations EL A1 and EL A2"
infer _ctx (Select _x1 _x2) = Left $ OtherError ("(Select x1 x2) expected variable as argument, but got: " ++ show _x1 ++ " and " ++ show _x2)

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

