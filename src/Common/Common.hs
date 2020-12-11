{-# Options -Wall -Wname-shadowing #-}

module Common.Common where


import Latte.Parsing.AbsLatte
import Common.ErrorPositions




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
