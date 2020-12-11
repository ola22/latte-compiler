{-# Options -Wall -Wname-shadowing #-}

module SemanticChecker where


import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Except


import Latte.Parsing.LexLatte
import Latte.Parsing.AbsLatte
import Latte.Parsing.ErrM
import Common.ErrorPositions
import Common.Common



type ParseType a = [Token] -> Err a

-- function environment (func id -> type)
type FunStore = (M.Map Ident (Type ErrorPos))
-- variable environment (var id -> (type, outside block))
type VarStore = (M.Map Ident (Type ErrorPos, Bool))

-- store for program checking (func env, var env, func type)
-- where 'func typ' is actually considered function's type, used for
-- checking return
type ProgChecker = StateT (FunStore, VarStore, Type ErrorPos) (ExceptT String IO)




--------------------------------------------------- PREPEARING FUNCS ENVIRONMENT -----------------------------------------------------


-- Function returns a list of arguments' types from given
-- list of arguments.
getArgsTypes :: [Arg ErrorPos] -> [Type ErrorPos] -> [Type ErrorPos]
getArgsTypes [] types = types
getArgsTypes ((Arg _ typ _):rest) types = 
    getArgsTypes rest (types ++ [typ])


-- Function prepears the environment for all functions.
-- Function returns pair (funcs, string), where string is an optional
-- error and funcs is an environment for functions. It contains all function
-- declarations.
getFuncs :: [TopDef ErrorPos] -> FunStore -> Bool -> (FunStore, Maybe String)
getFuncs [] funcs True = (funcs, Nothing)
getFuncs [] _ False = (M.empty, Just "No main function in program.")
getFuncs ((FnDef pos typ iden args _):rest) funcs is_main = 
    let func_typ = Fun pos typ (getArgsTypes args []) in
        -- multiple declaration of the same function
        if (M.member iden funcs) 
            then (funcs, Just $ "In line " ++ getLineNumber pos ++ ":"
                ++ "\nMultiple declaration of function '" 
                ++ getName iden ++ "'")
        else (
            if ((getName iden) == "main") 
                then (
                    -- checking main's type
                    case typ of
                        (Int _) -> (getFuncs rest (M.insert iden func_typ funcs) True)
                        _ -> (funcs, Just $ "In line " ++ getLineNumber pos ++ ":"
                            ++ "\nInvalid type of main function. Required type: int "
                            ++ "but got: " ++ getTypeName typ)
                )
            else (getFuncs rest (M.insert iden func_typ funcs) is_main)
        )


-- Function adds library functions to functions' environment.
addLibFuncs :: FunStore -> FunStore
addLibFuncs funs = 
    let lib_funs = libFuns in
        M.union funs (M.fromList lib_funs)



--------------------------------------------------- ADDING FUNCTION'S ARGUMENTS TO ENV --------------------------------------------------


-- Function checks if given function's argument has valid type.
-- Invalid are Void and Fun.
checkArgType :: Ident -> Type ErrorPos -> ProgChecker ()
checkArgType _ (Int _) = return ()
checkArgType _ (Str _) = return ()
checkArgType _ (Bool _) = return ()
checkArgType iden (Void pos) = 
    throwError ("In line " ++ getLineNumber pos ++ ":\nInvalid argument's '" 
            ++ getName iden ++  "' type 'void'") 
checkArgType iden (Fun pos _ _) = 
    throwError ("In line " ++ getLineNumber pos ++ ":\nInvalid argument's '" 
            ++ getName iden ++  "' type") 


-- Function adds function's arguments to variables' environment.
-- It also checks theirs correctness.
-- być może z falsem będzie, ale wtedy dodając nie obchodzi mnie ten bool
addArgsToEnv :: [Arg ErrorPos] -> ProgChecker ()
addArgsToEnv [] = return ()
addArgsToEnv ((Arg pos typ iden):rest) = do
    (funs, args, f) <- get
    if (M.member iden args) then do 
        throwError ("In line " ++ getLineNumber pos 
            ++ ":\nMultiple declaration of argument '" 
            ++ getName iden ++ "' in function's arguments.")
    else do 
        checkArgType iden typ
        put (funs, (M.insert iden (typ, False) args), f)
        addArgsToEnv rest


----------------------------------------------- CHECKING FUNCTION'S BODY (STATEMENTS) ------------------------------------------------


-- Function checks correctness of declaring new variable with 
-- initial value.
checkInit :: Item ErrorPos -> Type ErrorPos -> ProgChecker ()
checkInit (Init pos iden e) decl_typ = do
    e_typ <- checkExp e
    if (typesEquals e_typ decl_typ) 
        then return ()
    else throwError ("In line " ++ getLineNumber pos 
        ++ ":\nInitial value's type of variable '" ++ getName iden 
        ++ "' doesn't match declared type.\nDeclared type: " 
        ++ getTypeName decl_typ ++ ", but got: " ++ getTypeName e_typ)
checkInit _ _ = return ()


-- Function adds varibles from given list od items to variables' environment.
addVarsToEnv :: Type ErrorPos -> [Item ErrorPos] -> ProgChecker ()
addVarsToEnv _ [] = return ()
addVarsToEnv typ (var:rest) = do
    (funs, vars, f) <- get
    let iden = getIden var
    let iden_from_map = M.lookup iden vars
    case iden_from_map of
        Just (_, True) -> 
            throwError ("In line " ++ getPosItem var
            ++ ":\nMultiple declaration of variable '" 
            ++ getName iden ++ "' inside one block.")
        _ -> case typ of
            Int _ -> put (funs, (M.insert iden (typ, True) vars), f)
            Str _ -> put (funs, (M.insert iden (typ, True) vars), f)
            Bool _ -> put (funs, (M.insert iden (typ, True) vars), f)
            Void _ -> throwError ("In line " ++ getPosItem var
                        ++ ":\nInvalid variable's '" ++ getName iden
                        ++ "' type void")
            Fun _ _ _ -> throwError ("In line " ++ getPosItem var
                        ++ ":\nInvalid variable's '" ++ getName iden ++ "' type") 
    case var of
        Init pos iden' e -> do
            checkInit (Init pos iden' e) typ
            addVarsToEnv typ rest
        _ -> addVarsToEnv typ rest
    

-- Checking semantic correctness of decrement and increment statements.
checkIncrAndDecr :: Ident -> String -> ErrorPos -> ProgChecker ()
checkIncrAndDecr iden operation pos = do
    (_, vars, _) <- get
    let var_typ = M.lookup iden vars
    case var_typ of
        Nothing -> 
            throwError ("In line " ++ getLineNumber pos ++ ": \nTrying to " 
                ++ operation ++ " undeclared variable '" ++ getName iden ++ "'")
        Just (decl_typ, _) -> do
            case decl_typ of
                Int _ -> return ()
                _ -> throwError ("In line " ++ getLineNumber pos ++ ": \nTrying to " 
                    ++ operation ++ " variable '" ++ getName iden 
                    ++ "',which is not of int type")


-- Checking semantic correctness of if and if-else statements.
checkIf :: ErrorPos -> Expr ErrorPos -> Stmt ErrorPos 
            -> Stmt ErrorPos -> ProgChecker ()
checkIf pos e stmt1 stmt2 = do
    e_typ <- checkExp e
    case e_typ of
        (Bool _) -> do
            checkStmt stmt1
            checkStmt stmt2
        _ -> throwError ("In line " ++ getLineNumber pos 
                ++ ":\nIn if statement given condition of wrong type."
                ++ "Expected: bool, but got: " ++ getTypeName e_typ)


-- Function marks all variables from varibles' environment
-- to know that they are from outside blocks. This operation is done
-- before going into inner block. After checking inner block
-- the variables' environment is restored.
markVarsAsOldLevel :: VarStore -> VarStore
markVarsAsOldLevel vars = 
    M.map (\(typ, _) -> (typ, False)) vars


-- Function checks correctness of given statement.
checkStmt :: Stmt ErrorPos -> ProgChecker ()
checkStmt (Empty _) = return ()
checkStmt (BStmt _ (Block _ stmts)) = do
    (funs, vars, fun) <- get
    let vars' = markVarsAsOldLevel vars
    put (funs, vars', fun)
    checkFuncBody vars stmts
checkStmt (Decl _ typ vars) = do
    addVarsToEnv typ vars 
    return ()
checkStmt (Ass pos iden e) = do
    (_, vars, _) <- get
    let var_typ = M.lookup iden vars
    case var_typ of
        Nothing -> 
            throwError ("In line " ++ getLineNumber pos 
                ++ ":\nTrying to assign undeclared variable '" 
                ++ getName iden ++ "'")
        Just (decl_typ, _) -> do
            e_typ <- checkExp e
            if (typesEquals decl_typ e_typ) then
                return ()
            else
                throwError ("In line " ++ getLineNumber pos 
                ++ ":\nTrying to assign value of type " 
                ++ getTypeName e_typ ++ " to variable '" ++ getName iden
                ++ "' of type " ++ getTypeName decl_typ)
checkStmt (Incr pos iden) = checkIncrAndDecr iden "increase" pos
checkStmt (Decr pos iden) = checkIncrAndDecr iden "decrease" pos
checkStmt (Ret pos e) = do
    e_typ <- checkExp e
    (_, _, fun_typ) <- get
    if (typesEquals e_typ fun_typ) 
        then return ()
    else throwError ("In line " ++ getLineNumber pos 
        ++ ":\nReturned value type " ++ getTypeName e_typ ++ 
        " doesn't match function's type " ++ getTypeName fun_typ)
checkStmt (VRet pos) = do
    (_, _, fun_typ) <- get
    if (typesEquals (Void Nothing) fun_typ) 
        then return ()
    else throwError ("In line " ++ getLineNumber pos 
        ++ ":\nReturned value type void doesn't match function's type " 
        ++ getTypeName fun_typ)
checkStmt (Cond pos e stmt) = checkIf pos e stmt (Empty Nothing)
checkStmt (CondElse pos e stmt1 stmt2) = 
    checkIf pos e stmt1 stmt2
checkStmt (While pos e stmt) = do
    e_typ <- checkExp e
    case e_typ of
        (Bool _) -> checkStmt stmt
        _ -> throwError ("In line " ++ getLineNumber pos 
                ++ ":\nIn if statement given condition of wrong type."
                ++ "Expected type: bool, but got: " ++ getTypeName e_typ)
checkStmt (SExp _ e) = do
    _ <- checkExp e
    return ()



------------------------------------------------------ CHECKING EXPRESSIONS --------------------------------------------------


-- Function checks if arguments used in function's call are of the same types, as
-- in function's declaration.
checkArgsTypes :: ErrorPos -> [Type ErrorPos] -> [Expr ErrorPos] 
                -> Integer -> ProgChecker ()
checkArgsTypes _ [] [] _ = return () 
checkArgsTypes pos (arg_typ:types) (e:exprs) num = do
    e_typ <- checkExp e
    if (typesEquals e_typ arg_typ) then 
        checkArgsTypes pos types exprs (num + 1)
    else
        throwError ("In line " ++ getLineNumber pos 
            ++ ":\nIn function's application wrong type of argument number " 
            ++ show num ++ " . Expected type: " ++ getTypeName arg_typ 
            ++ ", but got: " ++ getTypeName e_typ)
checkArgsTypes _ _ _ _ = return ()


-- Function checks if number of arguments used in function's call is the
-- same as in function's declaration.
checkFuncArgs :: Type ErrorPos -> ErrorPos -> [Expr ErrorPos] -> ProgChecker ()
checkFuncArgs (Fun _ _ arg_types) pos args = do
    let len1 = length arg_types
    let len2 = length args
    if (len1 /= len2) then 
        throwError ("In line " ++ getLineNumber pos 
            ++ ":\nWrong number of arguments in function's application. Given " 
            ++ show len2 ++ " arguments instead of " ++ show len1)
    else do
        checkArgsTypes pos arg_types args 1
checkFuncArgs _ _ _ = return ()


-- Fucntion checks if boolean operations are applied to expr of boolean 
-- type.
checkBooleanOperators :: ErrorPos -> Expr ErrorPos -> Expr ErrorPos 
                    -> String -> ProgChecker (Type ErrorPos)
checkBooleanOperators pos e1 e2 op = do
    e1_typ <- checkExp e1
    e2_typ <- checkExp e2
    if (typesEquals e1_typ e2_typ) then do
        -- we know that types are the same
        case e1_typ of
            (Bool _) -> return e1_typ
            _ -> throwError ("In line " ++ getLineNumber pos
                ++ ":\nTrying to apply '" ++ op 
                ++ "' operation to expressions, which are not boolean type")
    else throwError ("In line " ++ getLineNumber pos
                ++ ":\nMismatched expressions types (" ++ getPosType e1_typ
                ++ ", " ++ getTypeName e2_typ ++ ") in '" ++ op ++ "' operation")


-- Function checks correctness of given comparison operations,
-- which are applied to given expressions.
checkRelOperators :: ErrorPos -> Expr ErrorPos -> Expr ErrorPos -> 
                    RelOp ErrorPos -> ProgChecker (Type ErrorPos)
checkRelOperators pos e1 e2 op = do
    e1_typ <- checkExp e1
    e2_typ <- checkExp e2
    if (typesEquals e1_typ e2_typ) then do
        case op of
            EQU _ -> do
                case e1_typ of
                    (Bool _) -> return (Bool pos)
                    (Str _) -> return (Bool pos)
                    (Int _) -> return (Bool pos)
                    _ -> throwError ("In line " ++ getLineNumber pos 
                                ++ ":\nTrying apply 'eq' operation to expressions, " 
                                ++ "which are not booleans, ints or strings ")
            NE _ -> do
                case e1_typ of
                    (Bool _) -> return (Bool pos)
                    (Str _) -> return (Bool pos)
                    (Int _) -> return (Bool pos)
                    _ -> throwError ("In line " ++ getLineNumber pos 
                                ++ ":\nTrying apply '!=' operation to expressions," 
                                ++ " which are not booleans, ints or strings") 
            _ -> do
                case e1_typ of
                    (Int _) -> return (Bool pos)
                    _ -> throwError ("In line " ++ getLineNumber pos 
                                ++ ":\nTrying apply rel operation to expressions," 
                                ++ " which are not ints") 
    else throwError ("In line " ++ getLineNumber pos
                    ++ ":\nMismatched expressions' types (" ++ getTypeName e1_typ 
                    ++ ", " ++ getTypeName e2_typ ++ ") in comparison operation")


-- Function checks correctness of addition and subtraction operations.
-- Addition can also be applied to strings.
checkAddAndMinus :: Expr ErrorPos -> ProgChecker (Type ErrorPos)
checkAddAndMinus (EAdd pos e1 op e2) = do
    e1_typ <- checkExp e1
    e2_typ <- checkExp e2
    if (typesEquals e1_typ e2_typ) then do
        -- we know that types are the same
        case op of
            Plus _ -> case e1_typ of
                (Int _) -> return e1_typ
                (Str _) -> return e1_typ
                _ -> throwError ("In line " ++ getLineNumber pos 
                            ++ ":\nTrying apply addition operation to expressions," 
                            ++ " which are not integers or strings")
            Minus _ -> case e1_typ of
                (Int _) -> return e1_typ
                _ -> throwError ("In line " ++ getLineNumber pos
                            ++ ":\nTrying apply subtraction operation to expressions," 
                            ++ " which are not ints")
    else throwError ("In line " ++ getLineNumber pos ++ ":\nMismatched expressions' types(" 
                    ++ getTypeName e1_typ ++ ", " ++ getTypeName e2_typ 
                    ++ ") in addition/subtraction operation")
checkAddAndMinus _ = return (Int Nothing)


-- Function checks correctness of 'not' and 'neg' operations.
checkNotAndNeg :: ErrorPos -> Expr ErrorPos -> Bool -> ProgChecker (Type ErrorPos)
checkNotAndNeg pos e neg = do
    e_typ <- checkExp e
    case e_typ of
        (Int _) -> do
            if (neg) then return e_typ 
            else throwError ("In line " ++ getLineNumber pos 
                ++ ":\nTrying to applay 'not' operation to expression," 
                ++ " which is not bool")
        (Bool _) -> do
            if (neg == False) then return e_typ
            else throwError ("In line " ++ getLineNumber pos 
                ++ ":\nTrying to applay 'neg' operation to expression," 
                ++ " which is int")
        _ -> throwError ("In line " ++ getLineNumber pos 
                ++ ":\nTrying to apply neg/not to expression, which is" 
                ++ " not bool/int")


-- Function checks correctness of given expression. It returns
-- expression's type.
checkExp :: Expr ErrorPos -> ProgChecker (Type ErrorPos) 
checkExp (EVar pos iden) = do
    (_, vars, _) <- get
    let var_typ = M.lookup iden vars
    case var_typ of
        Nothing -> 
            throwError ("In line " ++ getLineNumber pos 
                ++ ":\nTrying to use undeclared variable '" 
                ++ getName iden ++ "'")
        Just (decl_typ, _) -> return decl_typ
checkExp (ELitInt pos _) = return (Int pos) 
checkExp (ELitTrue pos) = return (Bool pos) 
checkExp (ELitFalse pos) = return (Bool pos) 
checkExp (EApp pos iden exprs) = do
    (funs, _, _) <- get
    let fun_typ = M.lookup iden funs
    case fun_typ of
        Nothing -> 
            throwError ("In line " ++ getLineNumber pos 
                ++ ":\nTrying to use undeclared function '" 
                ++ getName iden ++ "'")
        Just (Fun pos' ret_typ arg_types) -> do
            checkFuncArgs (Fun pos' ret_typ arg_types) pos exprs
            return ret_typ
        Just typ -> return typ
checkExp (EString pos _) = return (Str pos)
checkExp (Neg pos e) = checkNotAndNeg pos e True
checkExp (Not pos e) = checkNotAndNeg pos e False
checkExp (EMul pos e1 _ e2) = do
    e1_typ <- checkExp e1
    e2_typ <- checkExp e2
    case e1_typ of
        (Int _) -> do
            case e2_typ of
                (Int _) -> return e1_typ
                _ -> throwError ("In line " ++ getLineNumber pos 
                        ++ ":\nTrying apply multiplication to right" 
                        ++ " expression, which is not int") 
        _ -> throwError ("In line " ++ getLineNumber pos 
                        ++ ":\nTrying apply multiplication to left" 
                        ++ " expression, which is not int")
checkExp (EAdd pos e1 op e2) = checkAddAndMinus (EAdd pos e1 op e2)
checkExp (ERel pos e1 op e2) = checkRelOperators pos e1 e2 op
checkExp (EAnd pos e1 e2) = checkBooleanOperators pos e1 e2 "and"
checkExp (EOr pos e1 e2) = checkBooleanOperators pos e1 e2 "or"


-- Function checks correctness od whole function's body.
-- After checking given block of statements it restores old 
-- variables' environment, since variables declared in block are
-- not visible outside given block.
checkFuncBody :: VarStore -> [Stmt ErrorPos] -> ProgChecker ()
checkFuncBody vars [] = do
    (funs, _, typ) <- get
    put (funs, vars, typ)
    return ()
checkFuncBody vars (stmt:rest) = do
    checkStmt stmt
    checkFuncBody vars rest


--------------------------------------------------------- CHECKING RETURNS -------------------------------------------------------------

-- Function returns a pair of boolean and string. If
-- given statement contains a return statement fnction
-- returns True. Otherwise it returns False and maybe 
-- the error info.
checkStmtsRet :: Stmt ErrorPos -> (Bool, Maybe String)
checkStmtsRet (Empty _) = (False, Nothing)
checkStmtsRet (BStmt _ (Block _ stmts)) = 
    checkReturnsInFunction stmts
checkStmtsRet (Decl _ _ _) = (False, Nothing)
checkStmtsRet (Ass _ _ _) = (False, Nothing)
checkStmtsRet (Incr _ _) = (False, Nothing)
checkStmtsRet (Decr _ _) = (False, Nothing)
checkStmtsRet (Ret _ _) = (True, Nothing)
checkStmtsRet (VRet _) = (True, Nothing)
checkStmtsRet (Cond pos e stmt) = 
    let (ret, _) = checkStmtsRet stmt
    in if (ret == False)
            then (False, Nothing)
        else case e of
            (ELitTrue _) -> (True, Nothing)
            (ELitFalse _) -> (False, Just $ "In line " ++ getLineNumber pos 
                            ++ ":\nIn given if statement return will " 
                            ++ "never be reached")
            _ -> (False, Just $ "In line " ++ getLineNumber pos 
                            ++ ":\nIn given if statementnReturn may not " 
                            ++ "always be reached")
checkStmtsRet (CondElse pos e stmt1 stmt2) = let
    (ret1, _) = checkStmtsRet stmt1
    (ret2, _) = checkStmtsRet stmt2 in
    if (ret1) then
        case e of
            (ELitTrue _) -> (True, Nothing)
            _ -> if (ret2) then (True, Nothing)
                else (False, Just $ "In line " ++ getLineNumber pos 
                        ++ ":\nReturn not always reached: missing" 
                        ++ " return in else branch")
    else (
        if (ret2) then
            case e of
                (ELitFalse _) -> (True, Nothing)
                _ -> (False, Just $ "In line " ++ getLineNumber pos 
                        ++ ":\nReturn not always reached: missing " 
                        ++ " return in if branch")
        else (False, Nothing)
    )
checkStmtsRet (While _ e _) =
    case e of
        (ELitTrue _) -> (True, Nothing)
        _ -> (False, Nothing)
checkStmtsRet (SExp _ e) = 
    case e of
        EApp _ (Ident "error") _ -> (True, Nothing)
        _ -> (False, Nothing)


-- Function checks, if there is a return stateent in given
-- function's body
checkReturnsInFunction :: [Stmt ErrorPos] -> (Bool, Maybe String)
checkReturnsInFunction [] = (False, Nothing)
checkReturnsInFunction (stmt:rest) =
    let (stmt_ret, stmt_err) = checkStmtsRet stmt
        (next_ret, next_err) = checkReturnsInFunction rest
    in if (stmt_ret) then (stmt_ret, Nothing)
        else (
            if (next_ret) then (next_ret, Nothing)
            else
                case stmt_err of
                    Nothing -> (next_ret, next_err)
                    Just _ -> (stmt_ret, stmt_err) 
        )



--------------------------------------------------- CHECKING PROGRAM'S CORRECTNESS -----------------------------------------------------


-- Function checks given top level definition of type FnDef
checkFunction :: TopDef ErrorPos -> ProgChecker ()
checkFunction (FnDef pos typ iden args (Block _ stmts)) = do
    addArgsToEnv args
    (funs, vars, _) <- get
    put (funs, vars, typ)
    checkFuncBody vars stmts

    case typ of
        (Void _) -> return ()
        _ -> do
            -- checking returns in considered function
            let (ret, err) = checkReturnsInFunction stmts
            if (ret == False) then do
                case err of
                    Just e -> throwError e
                    Nothing -> throwError ("In line " ++ getLineNumber pos 
                        ++ ":\nFunction '" ++ getName iden 
                        ++ "' does not have return statement")
            else return ()


-- Function checks given top level definition
checkTopDef :: TopDef ErrorPos -> ProgChecker ()
checkTopDef top_def = do
    case top_def of
        (FnDef _ _ _ _ _) -> checkFunction top_def


-- Function checks all top level definitions of given program
checkProgramElems :: [TopDef ErrorPos] -> ProgChecker ()
checkProgramElems [] = return ()
checkProgramElems (el:rest) = do
    (funs, _, _) <- get
    put (funs, M.empty, (Void Nothing))
    checkTopDef el
    checkProgramElems rest


-- Function checks program's semantic correctness
checkProgram :: Program ErrorPos -> IO (Maybe String)
checkProgram (Program _ prog) = do
    let (funcs, err) = getFuncs prog M.empty False
    let funcs' = addLibFuncs funcs
    case err of
        Nothing -> do
            res <- runExceptT (runStateT (checkProgramElems prog) 
                            (funcs', M.empty, Void Nothing))
            case res of
                Left err1 -> return (Just err1)
                Right _ -> return Nothing
        Just err2 -> return (Just err2)
