module Context where

import Syntax

data ContextError
  = VariableNotFound Var
  | ContextIsEmpty String
  | OtherContextError  String
  deriving (Eq, Show)

lookup' :: Ctx -> Var -> Either ContextError Type
lookup' EmptyCtx _ = Left (ContextIsEmpty "cannot lookup in empty context")
lookup' (Ctx (simple, ticked)) x =
  case simpleLookup simple x of -- lookup in most recent simple context
    Just ty -> Right ty -- found 
    Nothing -> -- if not found, look in the list of contexts
      case firstHit ticked x of
        Just ty -> Right ty
        Nothing -> Left (VariableNotFound x)


-- Helper function to find first instance for the variable (ignoring ticks)
firstHit :: [(SCtx, ClockExpr)] -> Var -> Maybe Type
firstHit [] _ = Nothing
firstHit ((sctx, _) : rest) x =
  case simpleLookup sctx x of
    Just ty -> Just ty
    Nothing -> firstHit rest x

-- Helper function (basically same as Prelude lookup..)
simpleLookup :: SCtx -> Var -> Maybe Type
simpleLookup [] _     = Nothing
simpleLookup ((x ,ty):txys) key
  | x == key  = Just ty
  | otherwise = simpleLookup txys key

-- Add variable binding to simple
addVarBind :: Ctx -> Var -> Type -> Ctx
addVarBind EmptyCtx v ty                = Ctx ([(v, ty)], [])
addVarBind (Ctx (simple, ticked)) v ty  = Ctx ((v, ty):simple, ticked)

-- Add tick to context by moving all variable bindings to ticked 
addTick :: Ctx -> ClockExpr -> Ctx
addTick EmptyCtx cl               = Ctx ([], [([], cl)])
addTick (Ctx (simple, ticked)) cl = Ctx ([], (simple, cl):ticked)

-- Advance value of exist later
-- advance :: Ctx -> 
-- flip the addTick

-- Fix and box