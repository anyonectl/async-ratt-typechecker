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
infer :: Cxt -> Term -> Either TypeError Type
infer cxt (Variable x) =
  case lookup' cxt x of
    Right ty -> Right ty
    Left err -> Left (ContextError err)

infer cxt (Ann t ty) =
  check cxt t ty

infer cxt (Case t1 _ t2 _ t3) = do
  ty1 <- infer cxt t1
  if ty1 == bool then do 
      ty2 <- infer cxt t2
      ty3 <- infer cxt t3
      if ty2 == ty3
        then Right ty2
        else Left (TypeError "Case branches did not match type")
    else Left (TypeError "t1 in Case is not a boolean")

infer _ _ = Left (TypeError "TODO: implement the rest")

-- CHECK
check :: Cxt -> Term -> Type -> Either TypeError Type
check cxt (Case t1 _ t2 _ t3) ty =
  case (check cxt t1 bool, check cxt t2 ty, check cxt t3 ty) of
    (Right _, Right _, Right _) -> Right ty
    _                           -> Left (TypeError "Case branches failed to check") -- TODO expand to pinpoint which branch failed

-- Is this better option? 
-- check cxt (Case t1 _ t2 _ t3) ty = do
--   check cxt t1 bool
--   check cxt t2 ty
--   check cxt t3 ty
--   Right ty

check cxt t ty =
  case infer cxt t of
    Right ty' ->
      if ty == ty'
        then Right ty
        else Left (TypeMismatch { expected = ty, got = ty' })
    Left err -> Left err