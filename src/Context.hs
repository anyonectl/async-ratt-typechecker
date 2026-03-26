module Context where

lookup' :: Ctx -> Var -> Maybe Type
lookup' EmptyCtx _ = Nothing
lookup' (Cxt (sctx, cl)) x =
    case lookup x sctx of -- lookup in most recent simple context
        Just ty -> Just ty
        Nothing ->
    where 
         

lookup'' :: Ctx -> Var -> Maybe Type
lookup'' EmptyCtx _ = 

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