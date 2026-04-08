module TypeChecker where

import Syntax
import Context

data TypeError
  = TypeMismatch  { expected :: Type, got :: Type }
  | UnboundVar    String
  | ContextError  ContextError
  | TypeError     String
  deriving (Eq, Show)

-- INFER
infer :: Ctx -> Term -> Either TypeError Type
infer ctx (Variable x) =
  case lookup' ctx x of
    Right ty -> Right ty
    Left err -> Left (ContextError err)

infer ctx (Ann t ty) =
  check ctx t ty

infer ctx (Case t1 _ t2 _ t3) = do
  ty1 <- infer ctx t1
  if ty1 == bool then do 
      ty2 <- infer ctx t2
      ty3 <- infer ctx t3
      if ty2 == ty3
        then Right ty2
        else Left (TypeError "Case branches did not match type")
    else Left (TypeError "t1 in Case is not a boolean")

infer _ _ = Left (TypeError "TODO: implement the rest")

-- CHECK
check :: Ctx -> Term -> Type -> Either TypeError Type
check ctx (Case t1 _ t2 _ t3) ty =
  case (check ctx t1 bool, check ctx t2 ty, check ctx t3 ty) of
    (Right _, Right _, Right _) -> Right ty
    _                           -> Left (TypeError "Case branches failed to check") -- TODO expand to pinpoint which branch failed

-- Is this better option? 
-- check ctx (Case t1 _ t2 _ t3) ty = do
--   check ctx t1 bool
--   check ctx t2 ty
--   check ctx t3 ty
--   Right ty

check ctx t ty =
  case infer ctx t of
    Right ty' ->
      if ty == ty'
        then Right ty
        else Left (TypeMismatch { expected = ty, got = ty' })
    Left err -> Left err