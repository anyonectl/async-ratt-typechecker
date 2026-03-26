module Context where

import Syntax
{-
type SCxt = [(Var, Type)]
data Cxt = EmptyCtxt | Ctxt (SCxt, [(SCxt, ClockExpr)])
-}

data ContextError
  = VariableNotFound Var
  | ContextIsEmpty String
  | Other String
  deriving (Eq, Show)

lookup' :: Cxt -> Var -> Either ContextError Type
lookup' EmptyCxt _ = Left (ContextIsEmpty "cannot lookup in empty context")
lookup' (Cxt (simple, ticked)) x =
  case simpleLookup simple x of -- lookup in most recent simple context
    Just ty -> Right ty
    Nothing -> 
      case firstHit x ticked of
        Just ty -> Right ty
        Nothing -> Left (VariableNotFound x)


-- Helper function to find first instance for the variable (ignoring ticks)
firstHit :: Var -> [(SCxt, ClockExpr)] -> Maybe Type
firstHit _ [] = Nothing
firstHit x ((scxt, _) : rest) =
  case simpleLookup scxt x of
    Just ty -> Just ty
    Nothing -> firstHit x rest

-- Helper function (basically same as Prelude lookup..)
simpleLookup :: SCxt -> Var -> Maybe Type
simpleLookup [] _     = Nothing 
simpleLookup ((x ,ty):xtys) key
  | x == key  = Just ty 
  | otherwise = simpleLookup xtys x

-- -- Add variable binding  x : A to a typing context
-- addVarBind :: Var -> Type -> Ctx -> Ctx
-- addVarBind v t (simple, ticked) = ((v, t) : simple, ticked)

-- -- Add a tick on some clock to a typing context
-- addTick :: ClockExpr -> Ctx -> Ctx
-- addTick cl (simple, ticked) = ([], (cl, simple) : ticked)

-- -- Advance value of exist later
-- flip the addTick

-- Fix and box

-- -- lookupType :: Var -> Ctx -> Type
-- -- lookupType v ctx = ???