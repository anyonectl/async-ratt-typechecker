module Context where

import Syntax
import ErrorTypes
import Data.Function ((&))

type SimpleCtx = [(Var, Type)]

newtype Ctx = Ctx (SimpleCtx, [(ClockExpr, SimpleCtx)]) -- if you read from right to left: SimpleCtx, then a tick, then a new SimpleCtx

isEmptyCtx :: Ctx -> Bool
isEmptyCtx (Ctx ([], [])) = True
isEmptyCtx _              = False

isTickfree :: Ctx -> Bool
isTickfree (Ctx (_, []))  = True
isTickfree _              = False

-- Extend the typing context with a variable binding
-- Needs to make sure that the variable is not already in the (simple) context
-- Should it also make sure that the variable is not already in the ticked contexts?
addVar :: Var -> Type -> Ctx -> Ctx
addVar var ty (Ctx ([], []))            = Ctx ([(var, ty)], [])
addVar var ty (Ctx (simple, ticked))    = Ctx (update simple, ticked)
    where
        update []       = [(var, ty)]
        update ((v, t) : rest)
            | v == var  = (var, ty) : rest
            | otherwise = (v, t) : update rest

-- Extend the typing context with a tick on some clock
addTick :: ClockExpr -> Ctx -> Ctx
addTick cl (Ctx ([], []))           = Ctx ([], [(cl, [])])
addTick cl (Ctx (simple, ticked))   = Ctx ([], (cl, simple) : ticked)

lookupType :: Var -> Ctx -> Either ContextError Type
lookupType _   (Ctx ([], []))           = Left (ContextIsEmpty "cannot lookup in empty context")
lookupType var (Ctx (simple, ticked))   =
    case lookup var simple of
        Just ty -> Right ty
        Nothing -> searchTicked var ticked
  where
    -- Search for variable in ticked contexts (variable must have stable type)
    searchTicked :: Var -> [(ClockExpr, SimpleCtx)] -> Either ContextError Type
    searchTicked var' []                    = Left (VariableNotFound var')
    searchTicked var' ((_, simple') : rest) =
        case lookup var' simple' of
            Just ty | isStableType ty -> Right ty -- only stable types can cross ticks
            Just ty -> Left (UnstableType var' ty)
            Nothing -> searchTicked var' rest

-- Remove all ticks and non-stable variable bindings from a typing context
-- But maybe this is unnecessarily inefficient...?
stableCtx :: Ctx -> Ctx -- (Ctx ([(var, ty)], (tick, sc)))
stableCtx (Ctx ([], [])) = Ctx ([], [])
stableCtx (Ctx (sc, ticks)) =
    Ctx(concatMap (filter (\(_, ty) -> isStableType ty)) (sc : map snd ticks), [])

-- Remove the latest tick and all variable bindings added after that tick
-- needed for select, adv
preTick :: Ctx -> Either ContextError Ctx -- need to also check if a term has a specific clock ..
preTick (Ctx (_, (_, simple) : rest)) = Right (Ctx (simple, rest))
preTick _                             = Left NoTickInContext


-- Takes a context and a term
-- Checks if there is a previous tick on a clock belonging to a specific variable
-- Needed for (Adv x) and (Select v1 v2) where we can only typecheck if the previous tick belonged to a clock on the same term (variable) as the one advanced/selected
preTickOnSpecificClock :: Ctx -> Term -> Either ContextError Ctx
preTickOnSpecificClock (Ctx (_unticked, (Clock x1, simple) : rest)) x2 =
    if x1 == x2 
        then Right (Ctx (simple, rest))
        else Left NoTickInContext
preTickOnSpecificClock _ctx _t  = Left $ OtherContextError "Prev tick in context was not on variable x"