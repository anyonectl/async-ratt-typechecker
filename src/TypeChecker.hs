module TypeChecker where

import Syntax
import Context
import Data.Data (typeOf2)

-- from the paper on Bidirectional Typing Rules
data TypeError -- Should all errors be in Syntax.hs, or have errors for type checker in Typechecker.hs?
    = TypeMismatch { expected :: Type, got :: Type }
    | UnboundVar     String     -- variable not in context
    | CheckInferFail Term Type  -- inferred type didn't match
    | Other string -- for now we can add a message to the error and then generalise them as we go.. 
    -- ... probably add more later

-- INFER
-- “t’s type can be inferred to be τ in context Γ”
infer :: Ctx -> Term -> Either TypeError Type

infer ctx (Var x) =
  case lookup'' x ctx of
    Just typ -> Right ty
    Nothing -> Left (UnboundVar x)

infer ctx (Ann t ty) =
  check ctx t ty

infer ctx (If t1 t2 t3) = do
  ty1 <- infer ctx t1
  case ty1 of
    BoolTy -> do
        ty2 <- infer ctx t2
        ty3 <- infer ctx t3
        if ty2 == ty3
          then Right ty2
          else Left Other "if-branches did not match type"
    _ -> Left Other "Could not infer a boolean condition"

infer ctx (Case t1 x1 t2 x2 t3) = do
  ty1 <- infer ctx t1
  case ty1 of
    bool -> do                    -- Check if ty1 is of type 'bool'
      ty2 <- infer ctx t2
      ty3 <- infer ctx t3
      if ty2 == ty3
        then Right ty2           -- Both branches return the same type
        else Left Other "Case branches did not match type"
    _ -> Left Other "Expression in Case must be of boolean type"

-- CHECK
-- “t can be checked to have the given type”
check :: Ctx -> Term -> Type -> Either TypeError Type

check ctx (If t1 t2  t3) ty =
  case (check ctx t1 BoolTy, check ctx t2 ty, check ctx t3 ty) of
    (Just _, Just _, Just _) -> Just ty
    _                        -> Nothing

check ctx t ty = -- checking mode subsumes inference mode
  case infer ctx t of
  Just ty' -> if ty == ty' then Just ty
              else Nothing
  _        -> Nothing