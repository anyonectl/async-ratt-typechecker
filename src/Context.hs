module Context where

import Syntax

type SimpleCtx = [(Var, Type)]

newtype Ctx =  Ctx (SimpleCtx, [(ClockExpr, SimpleCtx)])

data ContextError
    = VariableNotFound Var
    | ContextIsEmpty String
    | UnstableType Var Type
    | NoTickInContext
    | OtherContextError String
    deriving (Eq, Show)

isTickfree :: Ctx -> Bool
isTickfree (Ctx ([], [])) = True
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
addTick cl (Ctx ([], []))             = Ctx ([], [(cl, [])])
addTick cl (Ctx (simple, ticked))   = Ctx ([], (cl, simple) : ticked)

lookupType :: Var -> Ctx -> Either ContextError Type
lookupType _   (Ctx ([], []))           = Left (ContextIsEmpty "cannot lookup in empty context")
lookupType var (Ctx (simple, ticked)) =
    case lookup var simple of
        Just ty -> Right ty
        Nothing -> searchTicked var ticked
  where
    -- Search for variable in ticked contexts (variable must have stable type)
    searchTicked :: Var -> [(ClockExpr, SimpleCtx)] -> Either ContextError Type
    searchTicked var []                    = Left (VariableNotFound var)
    searchTicked var ((_, simple') : rest) =
        case lookup var simple' of
            Just ty | isStableType ty -> Right ty -- only stable types can cross ticks
            Just ty -> Left (UnstableType var ty)
            Nothing -> searchTicked var rest

-- Remove all ticks and non-stable variable bindings from a typing context
-- But maybe this is unnecessarily inefficient...?
stableCtx :: Ctx -> Ctx
stableCtx (Ctx ([], [])) = Ctx ([], [])
stableCtx ctx            = Ctx (ctx & removeTicks & removeNonStable, [])
    where
        removeTicks (Ctx ([], []))                  = []
        removeTicks (Ctx (sc, []))                  = sc
        -- E.g. here, we concatenate lists, where the first list grows after each recursive step... so the running time also grows. 
        -- Would it be better to prepend, even though newest variables will then be at the end of the list?
        removeTicks (Ctx (sc, (tick, sc') : rest))  = sc ++ removeTicks (Ctx (sc', rest))
        
        removeNonStable []      = []
        removeNonStable ((x, ty) : rest)
            | isStableType ty   = (x, ty) : removeNonStable rest
            | otherwise         = removeNonStable rest

-- Remove the latest tick and all variable bindings added after that tick
-- needed for select, adv
preTick :: Ctx -> Either ContextError Ctx
preTick (Ctx (_, (_, simple) : rest)) = Right (Ctx (simple, rest))
preTick _                             = Left NoTickInContext

-- Advance value of exist later
-- advance :: Ctx -> 
-- flip the addTick

-- Fix and box