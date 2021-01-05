{-# Options -Wall -Wname-shadowing #-}

module Common.Common where

import qualified Data.Map.Strict as M


import Latte.Parsing.AbsLatte
import Common.ErrorPositions



type FuncTypMap = M.Map Ident (Type ErrorPos)




-- Function returns name of given varible/function.
getName :: Ident -> String
getName (Ident name) = name


-- Function returns item's identificator.
getIden :: Item ErrorPos -> Ident
getIden (Init _ iden _) = iden
getIden (NoInit _ iden) = iden


-- Function returns type name from given type.
getTypeName:: Type ErrorPos -> String
getTypeName (Int _) = "int"
getTypeName (Str _) = "string"
getTypeName (Bool _) = "bool"
getTypeName (Void _) = "void"
getTypeName (Fun _ _ _) = "function"


-- If given expression is not function application
-- and is not variable function returns its type, 
-- otherwise it returns Nothing.
getType :: Expr ErrorPos -> Maybe (Type ErrorPos)
getType (EVar _ _) = Nothing
getType (ELitInt pos _) = Just (Int pos)
getType (ELitTrue pos) = Just (Bool pos)
getType (ELitFalse pos) = Just (Bool pos)
getType (EApp _ _ _) = Nothing
getType (EString pos _) = Just (Str pos)
getType (Neg pos _) = Just (Int pos)
getType (Not pos _) = Just (Bool pos)
getType (EMul pos _ _ _) = Just (Int pos)
getType (EAnd pos _ _) = Just (Bool pos)
getType (EOr pos _ _) = Just (Bool pos)
getType (EAdd pos e1 op e2) = 
    case op of 
        Minus _ -> Just (Int pos)
        Plus _ -> 
            let typ = getType e1 
            in case typ of
                Just t -> Just t
                Nothing ->
                    let typ2 = getType e2 
                    in case typ2 of
                        Just t -> Just t
                        Nothing -> Nothing
getType (ERel pos e1 op e2) = 
    case op of
        LTH _ -> Just (Int pos)
        LE _ -> Just (Int pos)
        GTH _ -> Just (Int pos)
        GE _ -> Just (Int pos)
        EQU _ ->
            let typ = getType e1 
            in case typ of
                Just t -> Just t
                Nothing ->
                    let typ2 = getType e2 
                    in case typ2 of
                        Just t -> Just t
                        Nothing -> Nothing
        NE _ ->
            let typ = getType e1 
            in case typ of
                Just t -> Just t
                Nothing ->
                    let typ2 = getType e2 
                    in case typ2 of
                        Just t -> Just t
                        Nothing -> Nothing





-- Function returns inner expression of given expression
-- "inner" meaning literal, variable or function application
{-getInnerExpr :: Expr ErrorPos -> Expr ErrorPos
getInnerExpr (EVar _ _) = Nothing
getInnerExpr (ELitInt pos _) = Just (Int pos)
getInnerExpr (ELitTrue pos) = Just (Bool pos)
getInnerExpr (ELitFalse pos) = Just (Bool pos)
getInnerExpr (EApp _ _ _) = Nothing
getInnerExpr (EString pos _) = Just (Str pos)
getInnerExpr (Neg pos _) = Just (Int pos)
getInnerExpr (Not pos _) = Just (Bool pos)
getInnerExpr (EMul pos _ _ _) = Just (Int pos)
getInnerExpr (EAnd pos _ _) = Just (Bool pos)
getInnerExpr (EOr _ _ _) = Just (Bool pos)
getInnerExpr (EAdd _ e1 _ _) = getInnerExpr e1
getInnerExpr (ERel _ e1 _ _) = getInnerExpr e1-}


-- Function checks if given two types are the same.
typesEquals :: Type ErrorPos -> Type ErrorPos -> Bool
typesEquals (Int _) (Int _) = True
typesEquals (Str _) (Str _) = True
typesEquals (Bool _) (Bool _) = True
typesEquals (Void _) (Void _) = True
typesEquals (Fun _ _ _) (Fun _ _ _) = True
typesEquals _ _ = False


-- Function returns list of library functions. It's a list 
-- of pairs (library functions identificator, its type).
libFuns :: [(Ident, Type ErrorPos)]
libFuns = [((Ident "printInt"), (Fun Nothing (Void Nothing) [Int Nothing])),
            ((Ident "printString"), (Fun Nothing (Void Nothing) [Str Nothing])),
            ((Ident "error"), (Fun Nothing (Void Nothing) [])),
            ((Ident "readInt"), (Fun Nothing (Int Nothing) [])),
            ((Ident "readString"), (Fun Nothing (Str Nothing) []))]


-- Function creates map containing function's types
getFunctionsTypes :: FuncTypMap -> [TopDef ErrorPos] -> FuncTypMap
getFunctionsTypes m [] = m
getFunctionsTypes m ((FnDef _ typ iden _ _):rest) = 
    getFunctionsTypes (M.insert iden typ m) rest

