{-# Options -Wall -Wname-shadowing #-}

module Common.ErrorPositions where


import Latte.Parsing.AbsLatte



type ErrorPos = Maybe (Int, Int)



-- Function returns line number's string from 
-- given position
getLineNumber :: ErrorPos -> String
getLineNumber pos = 
    case pos of
        Just (line,_) -> show line
        Nothing -> "unknown"



-- All following functions return line number 
-- from given program's element.

getPosProg :: Program ErrorPos -> String
getPosProg (Program pos _) = getLineNumber pos


getPosTopDef :: TopDef ErrorPos -> String
getPosTopDef (FnDef pos _ _ _ _) = getLineNumber pos


getPosArg :: Arg ErrorPos -> String
getPosArg (Arg pos _ _) = getLineNumber pos


getPosBlock :: Block ErrorPos -> String
getPosBlock (Block pos _) = getLineNumber pos


getPosStmt :: Stmt ErrorPos -> String
getPosStmt (Empty pos) = getLineNumber pos
getPosStmt (BStmt pos _) = getLineNumber pos
getPosStmt (Decl pos _ _) = getLineNumber pos
getPosStmt (Ass pos _ _) = getLineNumber pos
getPosStmt (Incr pos _) = getLineNumber pos
getPosStmt (Decr pos _) = getLineNumber pos
getPosStmt (Ret pos _) = getLineNumber pos
getPosStmt (VRet pos) = getLineNumber pos
getPosStmt (Cond pos _ _) = getLineNumber pos
getPosStmt (CondElse pos _ _ _) = getLineNumber pos
getPosStmt (While pos _ _) = getLineNumber pos
getPosStmt (SExp pos _) = getLineNumber pos


getPosItem :: Item ErrorPos -> String
getPosItem (NoInit pos _) = getLineNumber pos
getPosItem (Init pos _ _) = getLineNumber pos


getPosType :: Type ErrorPos -> String
getPosType (Int pos) = getLineNumber pos
getPosType (Str pos) = getLineNumber pos
getPosType (Bool pos) = getLineNumber pos
getPosType (Void pos) = getLineNumber pos
getPosType (Fun pos _ _) = getLineNumber pos


getPosExpr :: Expr ErrorPos -> String
getPosExpr (EVar pos _) = getLineNumber pos
getPosExpr (ELitInt pos _) = getLineNumber pos
getPosExpr (ELitTrue pos) = getLineNumber pos
getPosExpr (ELitFalse pos) = getLineNumber pos
getPosExpr (EApp pos _ _) = getLineNumber pos
getPosExpr (EString pos _) = getLineNumber pos
getPosExpr (Neg pos _) = getLineNumber pos
getPosExpr (Not pos _) = getLineNumber pos
getPosExpr (EMul pos _ _ _) = getLineNumber pos
getPosExpr (EAdd pos _ _ _) = getLineNumber pos
getPosExpr (ERel pos _ _ _) = getLineNumber pos
getPosExpr (EAnd pos _ _) = getLineNumber pos
getPosExpr (EOr pos _ _) = getLineNumber pos


getPosAddOp :: AddOp ErrorPos -> String
getPosAddOp (Plus pos) = getLineNumber pos
getPosAddOp (Minus pos) = getLineNumber pos


getPosMulOp :: MulOp ErrorPos -> String
getPosMulOp (Times pos) = getLineNumber pos
getPosMulOp (Div pos) = getLineNumber pos
getPosMulOp (Mod pos) = getLineNumber pos


getPosRelOp :: RelOp ErrorPos -> String
getPosRelOp (LTH pos) = getLineNumber pos
getPosRelOp (LE pos) = getLineNumber pos
getPosRelOp (GTH pos) = getLineNumber pos
getPosRelOp (GE pos) = getLineNumber pos
getPosRelOp (EQU pos) = getLineNumber pos
getPosRelOp (NE pos) = getLineNumber pos











{-
getPosProg :: Program ErrorPos -> String
getPosProg (Program pos _) = 
    case pos of
        Just (line,_) -> show line
        Nothing -> "unknown"


getPosTopDef :: TopDef ErrorPos -> ErrorPos
getPosTopDef (FnDef pos _ _ _ _) = pos


getPosArg :: Arg ErrorPos -> ErrorPos
getPosArg (Arg pos _ _) = pos


getPosBlock :: Block ErrorPos -> ErrorPos
getPosBlock (Block pos _) = pos


getPosStmt :: Stmt ErrorPos -> ErrorPos
getPosStmt (Empty pos) = pos
getPosStmt (BStmt pos _) = pos
getPosStmt (Decl pos _ _) = pos
getPosStmt (Ass pos _ _) = pos
getPosStmt (Incr pos _) = pos
getPosStmt (Decr pos _) = pos
getPosStmt (Ret pos _) = pos
getPosStmt (VRet pos) = pos
getPosStmt (Cond pos _ _) = pos
getPosStmt (CondElse pos _ _ _) = pos
getPosStmt (While pos _ _) = pos
getPosStmt (SExp pos _) = pos


getPosItem :: Item ErrorPos -> ErrorPos
getPosItem (NoInit pos _) = pos
getPosItem (Init pos _ _) = pos


getPosType :: Type ErrorPos -> ErrorPos
getPosType (Int pos) = pos
getPosType (Str pos) = pos
getPosType (Bool pos) = pos
getPosType (Void pos) = pos
getPosType (Fun pos _ _) = pos


getPosExpr :: Expr ErrorPos -> ErrorPos
getPosExpr (EVar pos _) = pos
getPosExpr (ELitInt pos _) = pos
getPosExpr (ELitTrue pos) = pos
getPosExpr (ELitFalse pos) = pos
getPosExpr (EApp pos _ _) = pos
getPosExpr (EString pos _) = pos
getPosExpr (Neg pos _) = pos
getPosExpr (Not pos _) = pos
getPosExpr (EMul pos _ _ _) = pos
getPosExpr (EAdd pos _ _ _) = pos
getPosExpr (ERel pos _ _ _) = pos
getPosExpr (EAnd pos _ _) = pos
getPosExpr (EOr pos _ _) = pos


getPosAddOp :: AddOp ErrorPos -> ErrorPos
getPosAddOp (Plus pos) = pos
getPosAddOp (Minus pos) = pos


getPosMulOp :: MulOp ErrorPos -> ErrorPos
getPosMulOp (Times pos) = pos
getPosMulOp (Div pos) = pos
getPosMulOp (Mod pos) = pos


getPosRelOp :: RelOp ErrorPos -> ErrorPos
getPosRelOp (LTH pos) = pos
getPosRelOp (LE pos) = pos
getPosRelOp (GTH pos) = pos
getPosRelOp (GE pos) = pos
getPosRelOp (EQU pos) = pos
getPosRelOp (NE pos) = pos
-}
