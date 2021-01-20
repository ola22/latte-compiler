{-# Options -Wall -Wname-shadowing #-}

module Common.Common where

import qualified Data.Map.Strict as M


import Latte.Parsing.AbsLatte
import Common.ErrorPositions



type FuncTypMap = M.Map Ident (Type ErrorPos)
type StructTypMap = M.Map Ident [StructBody ErrorPos]




-- Function returns name of given varible/function.
getName :: Ident -> String
getName (Ident name) = name


-- Function returns item's identificator.
getIden :: Item ErrorPos -> Ident
getIden (Init _ iden _) = iden
getIden (NoInit _ iden) = iden


-- Function returns type name from given type.
getTypeName:: Type ErrorPos -> String
getTypeName (BltinType _ typ) = 
    case typ of
        (Int _) -> "int"
        (Str _) -> "string"
        (Bool _) -> "bool"
        (Void _) -> "void"
getTypeName (Fun _ _ _) = "function"
getTypeName (ArrType _ typ) =
    case typ of
        (BuiltinArr pos b_typ) -> "array of " ++ getTypeName (BltinType pos b_typ)
        (UserArr _ iden) -> "array of " ++ getName iden
getTypeName (UserType _ iden) = getName iden


-- If given expression is not function application
-- and is not variable function returns its type, 
-- otherwise it returns Nothing.
getType :: Expr ErrorPos -> Maybe (Type ErrorPos)
getType (EVar _ _) = Nothing
getType (ELitInt pos _) = Just (BltinType pos (Int pos))
getType (ELitTrue pos) = Just (BltinType pos (Bool pos))
getType (ELitFalse pos) = Just (BltinType pos (Bool pos))
getType (EApp _ _ _) = Nothing
getType (EString pos _) = Just (BltinType pos (Str pos))
getType (Neg pos _) = Just (BltinType pos (Int pos))
getType (Not pos _) = Just (BltinType pos (Bool pos))
getType (EMul pos _ _ _) = Just (BltinType pos (Int pos))
getType (EAnd pos _ _) = Just (BltinType pos (Bool pos))
getType (EOr pos _ _) = Just (BltinType pos (Bool pos))
getType (EAdd pos e1 op e2) = 
    case op of 
        Minus _ -> Just (BltinType pos (Int pos))
        Plus _ -> 
            let typ = getType e1 
            in case typ of
                Just t -> Just t
                Nothing ->
                    let typ2 = getType e2 
                    in case typ2 of
                        Just t -> Just t
                        Nothing -> Nothing
getType (ERel pos _ _ _) = 
    Just (BltinType pos (Bool pos))
getType (ENewArr pos typ _) = 
    case typ of
        BltinType _ b_typ -> 
            return (ArrType pos (BuiltinArr pos b_typ))
        UserType _ iden ->
            return (ArrType pos (UserArr pos iden))
        _ -> undefined
getType (ENewStruct _ typ) = Just typ
getType (EStructCoerce _ _) = Nothing
getType (EStructArrCoerce pos typ) = 
    Just (ArrType pos typ)
getType (EStructField _ _ _) = Nothing
getType (EArrAt _ _ _) = Nothing


-- Functions below check if given two types are the same.
builtinTypesEquals :: BuiltinType ErrorPos -> BuiltinType ErrorPos -> Bool
builtinTypesEquals (Int _) (Int _) = True
builtinTypesEquals (Str _) (Str _) = True
builtinTypesEquals (Bool _) (Bool _) = True
builtinTypesEquals (Void _) (Void _) = True
builtinTypesEquals _ _ = False


arrTypesEquals :: ArrayType ErrorPos -> ArrayType ErrorPos -> Bool
arrTypesEquals (BuiltinArr _ typ1) (BuiltinArr _ typ2) = 
    builtinTypesEquals typ1 typ2
arrTypesEquals (UserArr _ iden1) (UserArr _ iden2) = 
    (getName iden1) == (getName iden2)
arrTypesEquals _ _ = False


typesEquals :: Type ErrorPos -> Type ErrorPos -> Bool
typesEquals (BltinType _ typ1) (BltinType _ typ2) = 
    builtinTypesEquals typ1 typ2
typesEquals (UserType _ iden1) (UserType _ iden2) = 
    (getName iden1) == (getName iden2)
typesEquals (ArrType _ typ1) (ArrType _ typ2) = 
    arrTypesEquals typ1 typ2
typesEquals _ _ = False


-- Function returns list of library functions. It's a list 
-- of pairs (library functions identificator, its type).
libFuns :: [(Ident, Type ErrorPos)]
libFuns = [((Ident "printInt"), (Fun Nothing (BltinType Nothing (Void Nothing))
                    [BltinType Nothing (Int Nothing)])),
            ((Ident "printString"), (Fun Nothing (BltinType Nothing (Void Nothing)) 
                    [BltinType Nothing (Str Nothing)])),
            ((Ident "error"), (Fun Nothing (BltinType Nothing (Void Nothing)) [])),
            ((Ident "readInt"), (Fun Nothing (BltinType Nothing (Int Nothing)) [])),
            ((Ident "readString"), (Fun Nothing (BltinType Nothing (Str Nothing)) []))]


-- Function creates map containing function's types
getFunctionsTypes :: FuncTypMap -> StructTypMap -> [TopDef ErrorPos] 
                -> (FuncTypMap, StructTypMap)
getFunctionsTypes f_m s_m [] = (f_m, s_m)
getFunctionsTypes f_m s_m ((FnDef _ typ iden _ _):rest) = 
    getFunctionsTypes (M.insert iden typ f_m) s_m rest
getFunctionsTypes f_m s_m ((StructDef _ iden attrs):rest) =
    getFunctionsTypes f_m (M.insert iden attrs s_m) rest

