module TypeChecker where
import Syntax

-- “t’s type can be inferred to be τ in context Γ”
inferType :: Ctx -> Term -> Maybe Type
inferType ctx (If t1 t2 t3) = 
    case ( inferType ctx t1
         , inferType ctx t2
         , inferType ctx t3) of
        (Just Bool, Just ty2, Just ty3) ->
            if ty2 == ty3
                then Just ty2
                else Nothing
        _ -> Nothing
inferType ctx (Lam x t) = 
    case (inferType ((x, ???)::ctx) t) of
        Just ty2 -> Just (Fun ??? ty2)
        _        -> Nothing
inferType ctx (Var x) = lookup x ctx
inferType ctx (Ann t ty) = checkType ctz t ty

-- “t can be checked to have the given type”
checkType :: Ctx -> Term -> Type -> Type
checkType ctx t ty = case inferType ctx t of 
    Just ty' -> if ty == ty' then Just ty
                else Nothing
    _        -> Nothing
checkType ctx (If t1 t2  t3) ty =
    case ( checkType ctx t1 BoolTy
         , checkType ctx t2 ty
         , checkType ctx t3 ty ) of
        (Just BoolTy, Just ty2, Just ty3) -> Just ty
        _                                 -> Nothing
