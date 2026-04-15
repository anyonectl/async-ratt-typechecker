module TypeChecker where

import Syntax
import Context

data TypeError
  = TypeMismatch  { expected :: Type, got :: Type }
  | CtxError      ContextError
  | OtherError    String
  deriving (Eq, Show)

---------------------------
-- INFER
---------------------------
infer :: Ctx -> Term -> Either TypeError Type
infer ctx (Variable x) =
  case lookup' ctx x of
    Right ty -> Right ty
    Left err -> Left (CtxError err)

-- Case
infer ctx (Case t x1 t1 x2 t2) = do
  ty <- infer ctx t
  case ty of
    Sum a1 a2 -> do
      let ctx1 = addVarBind ctx x1 a1
      let ctx2 = addVarBind ctx x2 a2
      ty1 <- infer ctx1 t1
      ty2 <- infer ctx2 t2
      if ty1 == ty2
        then Right ty1
        else Left (OtherError "Case branches have different types")
    _ -> Left (OtherError "Case expected sum type as argument")
    
-- Let
infer ctx (Let x t1 t2) = do
  ty1 <- infer ctx t1
  let ctx' = addVarBind ctx x ty1
  infer ctx' t2

-- Projection (Prod type) 
infer ctx (Proj i t) = do
  ty <- infer ctx t
  case (i, ty) of 
    (Fst, Prod a _) -> Right a
    (Snd, Prod _ b) -> Right b
    _               -> Left (OtherError "Projection expected product type")

-- Succsessor
infer ctx (Suc t) = do
  ty <- infer ctx t
  if ty == Nat
    then Right Nat
    else Left (TypeMismatch { expected = Nat, got = ty })

-- Application
infer ctx (App f t) = do
  fTy <- infer ctx f
  case fTy of
    Fun a b -> do
      check ctx t a
      Right b
    _ -> Left (OtherError "Application expected function type")

-- Delay  
-- Advance
-- Advance AL
-- Select
-- Always
-- Unbox
-- Never
-- Wait
-- Read
-- Head
-- Tail
-- Cons

-- Add new implementations above this comment

-- Default
infer _ _ = Left (OtherError "Infer failed..")


---------------------------
-- CHECK
---------------------------
check :: Ctx -> Term -> Type -> Either TypeError Type

-- Case
check ctx (Case t x1 t1 x2 t2) ty = do
  arg <- infer ctx t
  case arg of
    Sum a1 a2 -> do               -- check that t is a sum type
      let ctx1 = addVarBind ctx x1 a1
      let ctx2 = addVarBind ctx x2 a2
      check ctx1 t1 ty
      check ctx2 t2 ty -- the error checking is better when using infer..? 
      Right ty                    -- then for each extended context branch, check that the type of t1 and t2 are ty
    _ -> Left (OtherError "Case argument is not a sum type")

-- Let
check ctx (Let x t1 t2) ty = do -- this is just like the Sestoft book (yay!)
  ty1 <- infer ctx t1
  let ctx' = addVarBind ctx x ty1
  check ctx' t2 ty

-- Lambda
check ctx (Lam x t) ty =
  case ty of
    Fun a b -> check (addVarBind ctx x a) t b
    _       -> Left (OtherError "Lambda expected function type")

-- Injection (Sum type)
check ctx (Inj rl t) ty =
  case ty of
    Sum a b -> case rl of
      InLeft  -> check ctx t a
      InRight -> check ctx t b
    _         -> Left (OtherError "Injection expected sum type")

-- Pair
check ctx (Pair t1 t2) ty =
  case ty of
    Prod a b -> do
      check ctx t1 a
      check ctx t2 b
      Right ty
    _       -> Left (OtherError "Pair expected a product type")

-- Delay
-- Advance
-- Advance AL
-- Select
-- Always
-- Unbox
-- Never
-- Wait
-- Read
-- Head
-- Tail
-- Cons

-- Add new implementations above this comment

-- Default
check ctx t ty =
  case infer ctx t of
    Right ty' | ty == ty' -> Right ty
    Right ty'             -> Left (TypeMismatch { expected = ty, got = ty' })
    Left err              -> Left err