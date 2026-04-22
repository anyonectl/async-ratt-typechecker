module ErrorTypes where

import Syntax

data ContextError
    = VariableNotFound Var
    | ContextIsEmpty String
    | UnstableType Var Type
    | NoTickInContext
    | OtherContextError String
    deriving (Eq, Show)

data TypeError
  = TypeMismatch        { term :: Term, expected :: Type, got :: Type }

  | ExpectedAL          { term :: Term, got :: Type }
  | ExpectedEL          { term :: Term, got :: Type }

  | ExpectedValue       { term :: Term, got :: Type }
  | ExpectedTwoValues   { term :: Term, got1 :: Type, got2 :: Maybe Type }

  | ExpectedBufferChannel Term
  | ExpectedPushChannel   Term

  | CtxError            ContextError
  
  | OtherError          String
  deriving (Eq)

instance Show TypeError where
    show (TypeMismatch t e g) = 
        "Type mismatch in term " ++ show t ++ 
        ": expected type " ++ show e ++ 
        ", but got type " ++ show g
    show (ExpectedAL t g) = 
        "Type mismatch in term " ++ show t ++
        ": expected type AL _" ++ 
        ", but got type " ++ show g 
    show (ExpectedEL t g) = 
        "Type mismatch in term " ++ show t ++
        ": expected type EL _" ++ 
        ", but got type " ++ show g
    show (ExpectedValue t g) = 
        "Type mismatch in term " ++ show t ++
        ": expected value type" ++ 
        ", but got type " ++ show g
    show (ExpectedTwoValues t g1 g2) =
        "Type mismatch in term " ++ show t ++
                ": " ++ errMsg g1 g2
            where
                errMsg g1 g2 = 
                    let msg1 = if not (isValue g1) 
                            then "(v1) expected value type, but got type " ++ show g1 
                            else ""
                        msg2 = if not (isValue g2') 
                            then "(v2) expected value type, but got type " ++ show g2'
                            else ""
                    in msg1 ++ msg2
    show (CtxError err) = "Context Error: " ++ show err
    show (OtherError msg) = "Error: " ++ msg