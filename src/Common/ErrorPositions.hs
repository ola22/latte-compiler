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
