module Context where

import Syntax
import ErrorTypes

type SimpleCtx = [(Var, Type)]

newtype Ctx = Ctx (SimpleCtx, [(ClockExpr, SimpleCtx)]) -- if you read from right to left: SimpleCtx, then a tick, then a new SimpleCtx
  deriving (Show)

isEmptyCtx :: Ctx -> Bool
isEmptyCtx (Ctx ([], [])) = True
isEmptyCtx _              = False

isTickfree :: Ctx -> Bool
isTickfree (Ctx (_, []))  = True
isTickfree _              = False

-- Extend the typing context with a variable binding
addVar :: Var -> Type -> Ctx -> Ctx
addVar x ty (Ctx (simple, ticked))    = Ctx ((x, ty) : simple, ticked)

-- Extend the typing context with a tick on some clock
addTick :: ClockExpr -> Ctx -> Ctx
addTick cl (Ctx (simple, ticked))   = Ctx ([], (cl, simple) : ticked)

lookupType :: Var -> Ctx -> Either ContextError Type
lookupType x (Ctx (simple, ticked))   =
  case lookup x simple of
    Just ty -> Right ty
    Nothing -> searchTicked x ticked
  where
    -- Search for variable in ticked contexts (variable must have stable type)
    searchTicked :: Var -> [(ClockExpr, SimpleCtx)] -> Either ContextError Type
    searchTicked var' []                    = Left (VariableNotFound var')
    searchTicked var' ((_, simple') : rest) =
      case lookup var' simple' of
        Just ty | isStableType ty -> Right ty -- only stable types can cross ticks
        Just _ -> Left (VariableNotFound var')
        Nothing -> searchTicked var' rest

-- Remove all ticks and non-stable variable bindings from a typing context
-- But maybe this is unnecessarily inefficient...?
stableCtx :: Ctx -> Ctx -- (Ctx ([(var, ty)], (tick, sc)))
stableCtx (Ctx (sc, ticks)) =
  Ctx (concatMap (filter (\(_, ty) -> isStableType ty)) (sc : map snd ticks), [])

-- Remove the latest tick and all variable bindings added after that tick
-- needed for select, adv
preTick :: Ctx -> Either ContextError Ctx -- need to also check if a term has a specific clock ..
preTick (Ctx (_, (_, simple) : rest)) = Right (Ctx (simple, rest))
preTick _                             = Left NoTickInContext

-- Used by Select: checks that the most recent tick matches a clock expression
-- Handles nested ClockUnions recursively 
preTickOnSpecificClock :: Ctx -> ClockExpr -> Either ContextError Ctx
preTickOnSpecificClock (Ctx (_, (cl, simple) : rest)) target
  | cl == target           = Right (Ctx (simple, rest)) -- if `cl == target` fails then haskell tries the next pattern
preTickOnSpecificClock ctx (ClockUnion cl1 cl2) = 
  case preTickOnSpecificClock ctx cl1 of
    Right ctx' -> Right ctx'
    Left _     -> preTickOnSpecificClock ctx cl2
preTickOnSpecificClock _ _  = Left NoTickInContext