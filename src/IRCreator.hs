{-# Options -Wall -Wname-shadowing #-}

module IRCreator where


import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Except

import Latte.Parsing.AbsLatte
import Common.ErrorPositions
import Common.Common





-- datatype for representing "registers" used in IR
data IRReg = 
    Const Integer | -- just constants
    Reg Integer | -- normal registers
    StrLoc Integer | -- string locations
    VarReg String -- variables register
    deriving (Eq, Ord, Show, Read)

-- datatype for conditions
data IRCond =  
    IRLoc IRReg | 
    IRRel IRRelOp IRReg IRReg 
    deriving (Eq, Ord, Show, Read)

-- arithmetic operations used in IR
data IRBinOp = 
    IRAdd | IRSub | IRTimes | IRDiv | IRMod
    deriving (Eq, Ord, Show, Read)
-- relation operations used in IR
data IRRelOp = 
    IRLTH | IRLE | IRGTH | IRGE | IREQU | IRNE
    deriving (Eq, Ord, Show, Read)

-- Boolean operator
data BoolOp = 
    AndOp | OrOp 
    deriving Show

-- Elements of "code" used for creating IR
data IRElem = 
    IROp IRReg IRBinOp IRReg IRReg | -- arithmetic op (res_reg, op, reg1, reg2)
    IRCall IRReg String [IRReg] | -- func call (res_reg, func_name, args_regs)
    AssignCond IRReg Integer | -- for making boolean expressions
    IRVarToReg IRReg IRReg | -- assigning var (var_reg, val_reg)
    IRVarFromReg IRReg IRReg | -- getting var's val to reg r (r, var_reg)
    GetPtr IRReg IRReg IRReg | -- getting pointer (to an array or struct)
    PutPtr IRReg IRReg IRReg  -- storeing pointer (to an array or struct)
    deriving Show

-- data for storeing labels used in blocks
data Label = 
    L String | -- block ended by unconditional jump
    NoJump | -- making no jump (movinf to next label)
    CondL IRCond String String | -- block ended by conditional jump
    RetL IRReg | -- block ended by return
    VRetL -- block ended by void return
    deriving (Eq, Ord, Show, Read)

-- string store (string -> string's location)
type StringStore = M.Map String IRReg

-- datatype representing one block
-- (start label, IR elements list, end label) 
type IRBlock =  (Label, [IRElem], Label) 

-- blocks env (blocks, first free label number, actually considered label)
type IRBlockStore = ([IRBlock], Integer, Label)

-- IR elems store 
-- (IR elems of act considered block, first free register numb)
type IRElemsStore = ([IRElem], Integer)

-- environment used for renaming variables, 
-- which has the same name
-- old_name -> (new_name, seed, type)
type VarEnv = M.Map String (String, Integer, Type ErrorPos)

-- funcs env (func name -> func type), type declared in Common.hs
-- type FuncTypMap = M.Map Ident (Type ErrorPos)

-- structures env (struct -> struct's atteibutes)
-- type StructTypMap = M.Map Ident [StructBody ErrorPos]

-- Store for creating IR representation
-- (string env, IR elems env, blocks env, funcs types, vars types and their new names)
type IRStore = StateT (StringStore, IRElemsStore, IRBlockStore, FuncTypMap, VarEnv, StructTypMap) 
                        (ExceptT String IO)




-- Function returns the index of given struct's field.
-- Giving "" iden will return number of all fields
getFieldIndexHelper :: [StructBody ErrorPos] -> Integer -> 
                    Ident -> IRStore (Integer)
getFieldIndexHelper [] idx _ = return (idx - 1)
getFieldIndexHelper ((Attr _ _ iden):rest) idx f_iden =
    if (getName iden == getName f_iden)
        then return (idx)
    else
        getFieldIndexHelper rest (idx + 1) f_iden


-- Returns index of a field of a given structure.
-- The indexes are 1, ... , n
getFieldIndex :: Ident -> Ident -> IRStore (Integer)
getFieldIndex s_iden f_iden = do
    (_, _, _, _, _, structs) <- get
    let res = M.lookup s_iden structs
    case res of
        Nothing -> undefined
        Just fields -> getFieldIndexHelper fields 1 f_iden


-- Function returns type of given array's elements.
getArrayAtType :: Expr ErrorPos -> VarEnv -> Type ErrorPos
getArrayAtType (EVar pos iden) vars = 
    let iden_from_map = M.lookup (getName iden) vars
    in case iden_from_map of
        Just (_, _, typ) -> 
            case typ of
                (ArrType _ typ') -> 
                    case typ' of
                        (BuiltinArr _ arr_typ) -> BltinType pos arr_typ
                        (UserArr _ typ_iden) -> UserType pos typ_iden
                _ -> undefined
        Nothing -> undefined -- there must be after sematic check
getArrayAtType _ _ = undefined


--  Function returns type of given structure's field
getFieldsTypeHelper :: [StructBody ErrorPos] -> Ident -> 
                        StructTypMap -> Type ErrorPos
getFieldsTypeHelper [] _ _ = undefined
getFieldsTypeHelper ((Attr _ typ iden):rest) f_iden structs =
    if (getName iden == getName f_iden)
        then typ
    else
        getFieldsTypeHelper rest f_iden structs


--  Function returns type of given structure's field
getFieldsType :: Ident -> Ident -> StructTypMap -> Type ErrorPos
getFieldsType s_iden f_iden structs = do
    let res = M.lookup s_iden structs
    case res of
        Nothing -> undefined
        Just fields -> getFieldsTypeHelper fields f_iden structs


-- Function returns type of given expression 
getExprType :: Expr ErrorPos -> FuncTypMap -> VarEnv -> 
                StructTypMap -> Type ErrorPos
getExprType e funcs vars structs = do
    let typ = getType e
    case typ of
        Just t -> t
        Nothing ->
            -- we know here that e is FAppl, Var, 
            -- Array's elem or structure's field
            case e of
                (EVar _ iden) -> do
                    case M.lookup (getName iden) vars of
                        Nothing -> undefined
                        Just (_, _, var_type) -> var_type
                (EApp _ iden _) -> do
                    case M.lookup iden funcs of
                        Nothing -> undefined
                        Just func_type -> func_type
                (EArrAt _ arr_e _) -> getArrayAtType arr_e vars
                (EStructCoerce pos name) -> do
                    case name of
                        EVar _ struct_name -> do
                            case M.lookup struct_name structs of
                                Nothing -> undefined
                                Just _ -> (UserType pos struct_name)
                        _ -> undefined
                (EStructField _ str field) -> do
                    let str_typ = getExprType str funcs vars structs
                    case str_typ of
                        (UserType _ s_iden) ->
                            getFieldsType s_iden field structs
                        (ArrType pos _) -> (BltinType pos (Int pos))
                        _ -> undefined
                _ -> undefined


-- Function adds given variable and its type to variables'
-- environment. If variable of given name was already declared 
-- it is renamed.
addVarToEnv :: Ident -> Type ErrorPos -> IRStore ()
addVarToEnv iden typ = do
    (str, ir, b, funcs, vars, structs) <- get
    let v_n = getName iden
    case M.lookup v_n vars of
        -- variable declared for the first time
        -- we don't have to rename it
        Nothing -> do
            let vars' = M.insert v_n (v_n, 1, typ) vars
            put (str, ir, b, funcs, vars', structs)
            return ()
        -- variable with this name already exists
        -- we change the name of new value
        Just (_, n, _) -> do
            let new_n = ".newn" ++ v_n ++ show n
            let vars' = M.insert v_n (new_n, n+1, typ) vars
            put (str, ir, b, funcs, vars', structs)
            return ()


--Function adds function's arguments from givem list
-- to variables' environment
addArgsToEnv :: [Arg ErrorPos] -> IRStore ()
addArgsToEnv [] = return ()
addArgsToEnv ((Arg _ typ iden):rest) = do
    addVarToEnv iden typ
    addArgsToEnv rest


-- Function returns current name of given variable
-- since it could have been renamed.
getCurVarName :: Ident -> IRStore (String)
getCurVarName iden = do
    (_, _, _, _, vars, _) <- get
    let v_n = getName iden
    case M.lookup v_n vars of
        -- variable should always be declared
        -- after semantic check
        Nothing -> undefined
        -- getting current variable name
        Just (cur_n, _, _) -> return (cur_n)


-- Function returns mul operation operator
getMulIROp :: MulOp ErrorPos -> IRBinOp
getMulIROp (Times _) = IRTimes
getMulIROp (Div _) = IRDiv
getMulIROp (Mod _) = IRMod


-- Function returns addition operator
getAddIROp :: AddOp ErrorPos -> IRBinOp
getAddIROp (Plus _) = IRAdd
getAddIROp (Minus _) = IRSub


-- Function returns register for string and 
-- adds this string to StringStore
getStrIRReg :: String -> IRStore (IRReg)
getStrIRReg s = do
    (s_store, (elems , n), b, f, v, structs) <- get
    case M.lookup s s_store of
        Nothing -> do
            let reg = StrLoc n
            put (M.insert s reg s_store, 
                (elems, n+1), b, f, v, structs)
            return (reg)
        Just reg -> return (reg)


------------------------------------------------- GENERATING IR FOR EXPRESSIONS -------------------------------------------------------------------


-- Function returns list of registers of function's arguments
-- and adds theirs IR elements to environment
getArgsRegsAndIRElems :: [Expr ErrorPos] -> IRStore ([IRReg])
getArgsRegsAndIRElems [] = return ([])
getArgsRegsAndIRElems (e:rest) = do
    reg <- generateExprIRElems e
    rest_regs <- getArgsRegsAndIRElems rest
    return ([reg] ++ rest_regs) 


-- Function generates all IR elements for given relop.
generateRelIRElems :: Expr ErrorPos -> IRStore (IRReg)
generateRelIRElems e@(ERel _ _ _ _) = do
    (s, (elems, n), (blocks, l, lebel1), funcs, vars, structs) <- get
    let res_reg = Reg n
    put (s, (elems, n + 1), (blocks, l + 3, lebel1), funcs, vars, structs)
    bool_blocks <- generateBoolExprIRElems e 
                ("lebel" ++ show l) ("lebel" ++ show (l+1))
    let b1 = (L ("lebel" ++ show l), [AssignCond res_reg 1], 
            L ("lebel" ++ show (l+ 2)))
    let b2 = (L ("lebel" ++ show (l + 1)), [AssignCond res_reg 0], 
            L ("lebel" ++ show (l+ 2)))
    (s', (_, n'), (_, l', _), funcs', vars', structs') <- get
    put (s', ([], n'), (bool_blocks ++ [b1] ++ [b2], l', 
        L ("lebel" ++ show (l+ 2))), funcs', vars', structs')
    return (res_reg)
generateRelIRElems _ = undefined


-- Function computes expression, that consists of constant
-- values only.
optimizeConstOp :: IRReg -> IRReg -> IRBinOp -> Maybe (IRReg)
optimizeConstOp r1 r2 op = 
    case r1 of 
        (Const x) -> 
            case r2 of 
                (Const y) -> 
                    case op of
                        IRAdd -> Just $ Const (x+y)
                        IRSub -> Just $ Const (x-y)
                        IRTimes -> Just $ Const (x*y)
                        IRDiv -> Just $ Const (x `div` y)
                        IRMod -> Just $ Const (x `mod` y)
                _ -> Nothing
        _ -> Nothing


-- Helper function for optimatizating function.
getBoolReg :: String -> Integer -> Integer -> Maybe (IRReg)
getBoolReg op x y = 
    if (((op == "or") && (x == 1 || y == 1)) ||
        ((op == "and") && (x == 1 && y == 1))) then
        Just $ Const 1
    else Just $ Const 0


-- Function optimizes two const boolean expressions.
optimizeConstBoolOp :: IRReg -> IRReg -> String -> Maybe (IRReg)
optimizeConstBoolOp r1 r2 op = 
    case r1 of
        (Const x) -> 
            case r2 of 
                (Const y) -> getBoolReg op x y
                _ -> Nothing
        _ -> Nothing


-- Function generates IR elements and blocks for given boolean expression.
-- It is needed to create comditional statements, that use less stack (they
-- use jumps instead). In all patterns l1 is label when we know e is true and
-- l2 is label when we know e is false
generateBoolExprIRElems :: Expr ErrorPos -> String -> String -> IRStore ([IRBlock])
generateBoolExprIRElems (EVar pos iden) l1 l2 = do
    (_, _, _, funcs', vars', structs') <- get
    let var_typ = getExprType (EVar pos iden) funcs' vars' structs'
    case var_typ of 
        (BltinType _ (Bool _)) -> do
            e_reg <- generateExprIRElems (EVar pos iden)
            (_, (elems, _), (blocks, _, lebel1), _, _, _) <- get
            let b = (lebel1, elems, CondL (IRLoc e_reg) l1 l2)
            return (blocks ++ [b])
        _ -> undefined
generateBoolExprIRElems (ELitTrue _) l1 _ = do
    (_, (elems, _), (blocks, _, lebel1), _, _, _) <- get
    let b = (lebel1, elems, L l1)
    return (blocks ++ [b])
generateBoolExprIRElems (ELitFalse _) _ l2 = do
    (_, (elems, _), (blocks, _, lebel1), _, _, _) <- get
    let b = (lebel1, elems, L l2)
    return (blocks ++ [b])
generateBoolExprIRElems (Not _ e) l1 l2 = 
    generateBoolExprIRElems e l2 l1
generateBoolExprIRElems (ERel _ e1 op e2) l1 l2 = do
    e1_reg <- generateExprIRElems e1
    e2_reg <- generateExprIRElems e2
    (_, (elems, _), (blocks, _, lebel1), _, _, _) <- get
    let res rel_op =
            let b = 
                    (lebel1, elems, 
                    CondL (IRRel rel_op e1_reg e2_reg) l1 l2)
            in return (blocks ++ [b])
    case op of
        LTH _ -> res IRLTH
        LE _ -> res IRLE
        GTH _ -> res IRGTH
        GE _ -> res IRGE
        EQU _ -> res IREQU
        NE _ -> res IRNE
generateBoolExprIRElems (EAnd _ e1 e2) l1 l2 = do
    (s', ir_e', (blocks', l', lebel1'), funcs', vars', structs') <- get
    -- l' - secondcondlebel
    put (s', ir_e', (blocks', l' + 1, lebel1'), funcs', vars', structs')
    blocks <- generateBoolExprIRElems e1 ("lebel" ++ show l') l2
    (s'', (_, n''), (_, l'', _), funcs'', vars'', structs'') <- get
    put (s'', ([], n''), (blocks, l'', 
        L ("lebel" ++ show l')), funcs'', vars'', structs'')
    generateBoolExprIRElems e2 l1 l2
generateBoolExprIRElems (EOr _ e1 e2) l1 l2 = do
    (s', ir_e', (blocks', l', lebel1'), funcs', vars', structs') <- get
    -- l' - secondcondlebel
    put (s', ir_e', (blocks', l' + 1, lebel1'), funcs', vars', structs')
    blocks <- generateBoolExprIRElems e1 l1 ("lebel" ++ show l')
    (s'', (_, n''), (_, l'', _), funcs'', vars'', structs'') <- get
    put (s'', ([], n''), (blocks, l'', 
        L ("lebel" ++ show l')), funcs'', vars'', structs'')
    generateBoolExprIRElems e2 l1 l2
generateBoolExprIRElems e l1 l2 = do
    e_reg <- generateExprIRElems e
    (_, (elems, _), (blocks, _, lebel1), _, _, _) <- get
    let b = (lebel1, elems, CondL (IRLoc e_reg) l1 l2)
    return (blocks ++ [b])


-- Functions generates IR elements for a boolean operation
-- (and/or)
genarateBoolOpIRElems :: Expr ErrorPos -> IRStore (IRReg)
genarateBoolOpIRElems e = do
    (s, (elems, n), (blocks, l, lebel1), funcs, vars, structs) <- get
    let res_reg = Reg n
    -- l - lebel we know e is true
    -- l+1 - we know e is false
    -- l+2 - moving with code
    put (s, (elems, n + 1), (blocks, l + 3, lebel1), funcs, vars, structs)
    bool_blocks <- generateBoolExprIRElems e 
            ("lebel" ++ show l) ("lebel" ++ show (l+1))
    -- block: we know it is true
    let b1 = (L ("lebel" ++ show l), [AssignCond res_reg 1],
             L ("lebel" ++ show (l+ 2)))
    -- block: we know it is false
    let b2 = (L ("lebel" ++ show (l + 1)), [AssignCond res_reg 0], 
            L ("lebel" ++ show (l+ 2)))
    (s', (_, n'), (_, l', _), funcs', vars', structs') <- get
    put (s', ([], n'), (bool_blocks ++ [b1] ++ [b2], l', 
        L ("lebel" ++ show (l+ 2))), funcs', vars', structs')
    return (res_reg)


-- Function geneartes IR elements for given addition operation
generateAddIRElems :: Expr ErrorPos -> AddOp ErrorPos -> IRReg 
                    -> IRReg -> IRStore (IRReg)
generateAddIRElems e1 op e1_reg e2_reg = do
    (s, (elems, n), b, funcs, vars, structs) <- get
    let res_reg = Reg n
    case op of
        Minus _ -> do
            let new_el = IROp res_reg IRSub e1_reg e2_reg
            put (s, (elems ++ [new_el], n + 1), b, funcs, vars, structs)
            return (res_reg)
        Plus _ -> do
            let typ = getExprType e1 funcs vars structs
            case typ of
                (BltinType _ (Int _))-> do
                    let new_el = IROp res_reg IRAdd e1_reg e2_reg
                    put (s, (elems ++ [new_el], n + 1), b, funcs, vars, structs)
                    return (res_reg)
                _ -> do
                    let new_el = IRCall res_reg "_strconcat" [e1_reg, e2_reg] 
                    put (s, (elems ++ [new_el], n + 1), b, funcs, vars, structs)
                    return (res_reg)


-- Function returns struct's identificator
-- of given type 
getStructIdent :: Type ErrorPos -> Ident
getStructIdent (UserType _ iden) = iden
getStructIdent _ = undefined


-- Function creates and adds to environment all
-- IR elements and blocks created for IR representation
-- from given expression.
generateExprIRElems :: Expr ErrorPos -> IRStore (IRReg)
generateExprIRElems (EVar _ iden) = do
    (s, (elems , n), b, funcs, vars, structs) <- get
    let r = Reg n
    v_n <- getCurVarName iden
    let new_el = IRVarFromReg r (VarReg v_n)
    put (s, (elems ++ [new_el], n + 1), b, funcs, vars, structs)
    return (r)
generateExprIRElems (ELitInt _ x) = return (Const x)
generateExprIRElems (ELitTrue _) = return (Const 1)
generateExprIRElems (ELitFalse _) = return (Const 0)
generateExprIRElems (EString _ s) = do
    reg <- getStrIRReg s
    return (reg)
generateExprIRElems (EApp _ iden exprs) = do
    args_regs <- getArgsRegsAndIRElems exprs
    (s, (elems, n), b, funcs, vars, structs) <- get
    let res_reg = Reg n
    let el = IRCall res_reg ("_" ++ (getName iden)) args_regs
    put (s, (elems ++ [el], n+1), b, funcs, vars, structs)
    return (res_reg)
generateExprIRElems (Neg _ e) = do -- ints
    e_reg <- generateExprIRElems e
    (s, (elems, n), b, funcs, vars, structs) <- get
    let res_reg = Reg n
    let new_el = IROp res_reg IRSub (Const 0) e_reg
    put (s, (elems ++ [new_el], n + 1), b, funcs, vars, structs)
    return (res_reg)
generateExprIRElems (Not _ e) = do -- bools
    e_reg <- generateExprIRElems e
    (s, (elems, n), b, funcs, vars, structs) <- get
    let res_reg = Reg n
    let new_el = IROp res_reg IRSub (Const 1) e_reg
    put (s, (elems ++ [new_el], n + 1), b, funcs, vars, structs)
    return (res_reg)
generateExprIRElems (EMul _ e1 op e2) = do
    e1_reg <- generateExprIRElems e1
    e2_reg <- generateExprIRElems e2
    -- optimizing, when both constat values
    let r = optimizeConstOp e1_reg e2_reg (getMulIROp op)
    case r of
        Just c_r -> return (c_r)
        Nothing -> do 
            (s, (elems, n), b, funcs, vars, structs) <- get
            let res_reg = Reg n
            let new_el = IROp res_reg (getMulIROp op) e1_reg e2_reg
            put (s, (elems ++ [new_el], n + 1), b, funcs, 
                vars, structs)
            return (res_reg)
generateExprIRElems (EAdd _ e1 op e2) = do
    e1_reg <- generateExprIRElems e1
    e2_reg <- generateExprIRElems e2
    -- optimizing, when both constat values
    let r = optimizeConstOp e1_reg e2_reg (getAddIROp op)
    case r of
        Just c_r -> return (c_r)
        Nothing -> do
            generateAddIRElems e1 op e1_reg e2_reg
generateExprIRElems e@(ERel _ _ _ _) =
    generateRelIRElems e
generateExprIRElems e@(EAnd _ e1 e2) = do
    (str, e', b, f, v, structs) <- get
    e1_r <- generateExprIRElems e1
    e2_r <- generateExprIRElems e2
    let r = optimizeConstBoolOp e1_r e2_r "and"
    put (str, e', b, f, v, structs)
    case r of
        Just c_r -> return c_r
        Nothing -> do
            genarateBoolOpIRElems e
generateExprIRElems e@(EOr _ e1 e2) = do
    (str, e', b, f, v, structs) <- get
    e1_r <- generateExprIRElems e1
    e2_r <- generateExprIRElems e2
    let r = optimizeConstBoolOp e1_r e2_r "or"
    put (str, e', b, f, v, structs)
    case r of
        Just c_r -> return c_r
        Nothing -> do
            genarateBoolOpIRElems e 
generateExprIRElems (ENewArr _ _ e) = do
    size <- generateExprIRElems e
    (s, (elems, n), b, funcs, vars, structs) <- get
    let res_reg = Reg n
    let temp_reg = Reg (n + 1)
    -- calling c calloc for arr.length + 1
    -- (+1 is for storeing length) 
    let el1 = IROp temp_reg IRAdd (Const 1) size
    let el2 = IRCall res_reg "_allocate" [temp_reg]
    -- storeing arr's length at 0 index
    let el3 = PutPtr res_reg (Const 0) size
    put (s, (elems ++ [el1] ++ [el2] ++ [el3], n+2), b, 
        funcs, vars, structs)
    return (res_reg)
generateExprIRElems (ENewStruct _ typ) = do
    size <- getFieldIndex (getStructIdent typ) (Ident "")
    (s, (elems, n), b, funcs, vars, structs) <- get
    let res_reg = Reg n
    let el = IRCall res_reg "_allocate" [(Const size)]
    put (s, (elems ++ [el], n+1), b, 
        funcs, vars, structs)
    return (res_reg)
generateExprIRElems (EStructCoerce _ _) = return (Const 0)
generateExprIRElems (EStructArrCoerce _ _) = return (Const 0)
generateExprIRElems (EStructField _ e f_iden) = do
    (_, _, _, funcs, vars, structs) <- get
    let e_typ = getExprType e funcs vars structs
    e_reg <- generateExprIRElems e
    -- an field can refer either to struct or an array
    case e_typ of
        (UserType _ s_iden) -> do
            idx <- getFieldIndex s_iden f_iden
            (s, (elems, n), b, funcs', vars', structs') <- get
            let res_reg = Reg n
            let el = GetPtr res_reg e_reg (Const (idx - 1))
            put (s, (elems ++ [el], n + 1), b, 
                funcs', vars', structs')
            return (res_reg)
        (ArrType _ _) -> do
            (s, (elems, n), b, funcs', vars', structs') <- get
            let res_reg = Reg n
            let el = GetPtr res_reg e_reg (Const 0)
            put (s, (elems ++ [el], n + 1), b, 
                funcs', vars', structs')
            return (res_reg)
        _ -> undefined
generateExprIRElems (EArrAt _ arr_e index_e) = do
    -- generating IR elems for array exp and array idx
    arr_reg <- generateExprIRElems arr_e
    index_reg <- generateExprIRElems index_e
    (s, (elems, n), b, funcs, vars, structs) <- get
    let res_reg = Reg n
    let ptr = Reg (n + 1)
    -- creating blocks for array reference
    -- counting index idx + 1 (cause arr[0] is arr's length)
    let el1 = IROp ptr IRAdd (Const 1) index_reg
    -- puting pointer to arr in res_reg
    let el2 = GetPtr res_reg arr_reg ptr
    put (s, (elems ++ [el1] ++ [el2], n+2),
         b, funcs, vars, structs)
    return (res_reg)


------------------------------------------------- GENERATING IR FOR STATEMENTS -------------------------------------------------------------------


-- Function genrates all necassary IR elements for given list
-- of items
generateDeclIRElems :: Type ErrorPos -> [Item ErrorPos] -> IRStore ()
generateDeclIRElems _ [] = return ()
generateDeclIRElems typ (el:rest) = do
    case el of
        NoInit _ iden -> do
            addVarToEnv iden typ
            v_n <- getCurVarName iden
            let el' = IRVarToReg (VarReg v_n) (Const 0)
            (s, (elems , n), b, funcs, vars, structs) <- get
            put (s, (elems ++ [el'], n), b, funcs, vars, structs)
        Init _ iden e -> do
            e_reg <- generateExprIRElems e
            addVarToEnv iden typ
            v_n <- getCurVarName iden
            let el' = IRVarToReg (VarReg v_n) e_reg
            (s, (elems , n), b, funcs, vars, structs) <- get
            put (s, (elems ++ [el'], n), b, funcs, vars, structs)
    generateDeclIRElems typ rest


-- Function genrates all necassary IR elements for given list
-- of items, which are strings
generateDeclIRElemsForStr :: [Item ErrorPos] -> IRStore ()
generateDeclIRElemsForStr [] = return ()
generateDeclIRElemsForStr (el:rest) = do
    case el of
        NoInit _ iden -> do
            addVarToEnv iden (BltinType Nothing (Str Nothing))
            v_n <- getCurVarName iden
            str_reg <- getStrIRReg ['"', '"']
            let el' = IRVarToReg (VarReg v_n) str_reg
            (s, (elems , n), b, f, v, structs) <- get
            put (s, (elems ++ [el'], n), b, f, v, structs)
        Init _ iden e -> do
            e_reg <- generateExprIRElems e
            addVarToEnv iden (BltinType Nothing (Str Nothing))
            v_n <- getCurVarName iden
            let el' = IRVarToReg (VarReg v_n) e_reg
            (s, (elems , n), b, f, v, structs) <- get
            put (s, (elems ++ [el'], n), b, f, v, structs)
    generateDeclIRElemsForStr rest


-- Function generates IR elements for each od 
-- statements from given list
generateBlockIRElems :: [Stmt ErrorPos] -> IRStore ()
generateBlockIRElems [] = return ()
generateBlockIRElems (stmt:rest) = do
    generateStmtIRElems stmt
    generateBlockIRElems rest


-- Function generates IR elements for assignment. The lvalue of assignment
-- can be a variable, an array reference to some element or struct's field
generateAssignIRElems :: Expr ErrorPos -> Expr ErrorPos -> IRStore ()
generateAssignIRElems iden_e e = do
    e_reg <- generateExprIRElems e
    case iden_e of
        (EVar _ iden) -> do
            (s, (elems, n), b, funcs, vars, structs) <- get
            v_n <- getCurVarName iden
            let new_el = IRVarToReg (VarReg v_n) e_reg
            put (s, (elems ++ [new_el], n), b, funcs, vars, structs)
            return ()
        (EStructField _ str_e f_iden) -> do
            (_, _, _, funcs, vars, structs) <- get
            let str_typ = getExprType str_e funcs vars structs
            str_reg <- generateExprIRElems str_e
            idx <- getFieldIndex (getStructIdent str_typ) f_iden
            (s, (elems, n), b, funcs', vars', structs') <- get
            let el = PutPtr str_reg (Const (idx - 1)) e_reg
            put (s, (elems ++ [el], n), b, funcs', vars', structs')
            return ()
        (EArrAt _ arr_e idx) -> do
            arr_reg <- generateExprIRElems arr_e
            idx_reg <- generateExprIRElems idx
            (s, (elems, n), b, funcs, vars, structs) <- get
            let temp1 = Reg n
            let el1 = IROp temp1 IRAdd (Const 1) idx_reg
            let el2 = PutPtr arr_reg temp1 e_reg
            put (s, (elems ++ [el1] ++ [el2], n + 1), b, 
                funcs, vars, structs)
            return ()
        _ -> undefined


-- Function creates and adds to environment all
-- IR elements and blocks created for IR representation
-- from given statement.
generateStmtIRElems :: Stmt ErrorPos -> IRStore ()
generateStmtIRElems (Empty _) = return ()
generateStmtIRElems (BStmt _ (Block _ stmts)) = do
    -- ending the IR block created so far
    (s, (elems, n), (blocks, l, lebel1), funcs, vars, structs) <- get
    let b = (lebel1, elems, NoJump)
    put (s, ([], n), (blocks ++ [b], l + 1, 
        L ("lebel" ++ show l)), funcs, vars, structs)
    -- starting new IR block for block of stmts
    generateBlockIRElems stmts
    (s', (elems', n'), (blocks', l', lebel1'), funcs', _, structs') <- get
    let b2 = (lebel1', elems', NoJump)
    -- restoring old variables env after exiting inner block
    put (s', ([], n'), (blocks' ++ [b2], l' + 1, 
        L ("lebel" ++ show l')), funcs', vars, structs')
    return ()
generateStmtIRElems (Decl _ typ items) = 
    case typ of
        BltinType _ (Str _) -> 
            generateDeclIRElemsForStr items
        typ' -> generateDeclIRElems typ' items
generateStmtIRElems (Ass _ iden_e e) = do
    generateAssignIRElems iden_e e
generateStmtIRElems (Incr _ iden) = do
    (s, (elems, n), b, funcs, vars, structs) <- get
    v_n <- getCurVarName iden
    let var_reg = VarReg v_n
    let el1 = IRVarFromReg (Reg n) var_reg
    let el2 = IROp (Reg (n+1)) IRAdd (Reg n) (Const 1)
    let el3 = IRVarToReg var_reg (Reg (n+1))
    put (s, (elems ++ [el1] ++ [el2] ++ [el3], n + 2), b, 
        funcs, vars, structs)
    return ()
generateStmtIRElems (Decr _ iden) = do
    (s, (elems, n), b, funcs, vars, structs) <- get
    v_n <- getCurVarName iden
    let var_reg = VarReg v_n -- register with given variable
    let el1 = IRVarFromReg (Reg n) var_reg
    let el2 = IROp (Reg (n+1)) IRSub (Reg n) (Const 1) 
    let el3 = IRVarToReg var_reg (Reg (n+1))
    put (s, (elems ++ [el1] ++ [el2] ++ [el3], n + 2), b, 
        funcs, vars, structs)
    return ()
generateStmtIRElems (Ret _ e) = do
    e_reg <- generateExprIRElems e
    (s, (elems, n), (blocks, l, lebel1), funcs, vars, structs) <- get
    let b = (lebel1, elems, RetL e_reg)
    put (s, ([], n), (blocks ++ [b], l + 1, 
        L ("lebel" ++ show l)), funcs, vars, structs)
    return ()
generateStmtIRElems (VRet _) = do
    (s, (elems, n), (blocks, l, lebel1), funcs, vars, structs) <- get
    let b = (lebel1, elems, VRetL)
    put (s, ([], n), (blocks ++ [b], l + 1, 
        L ("lebel" ++ show l)), funcs, vars, structs)
    return ()
generateStmtIRElems (CondElse _ e stmt1 stmt2) = do
    (s, ir_e, (blocks, l, lebel1), f, v, structs) <- get
    put (s, ir_e, (blocks, l + 3, lebel1), f, v, structs)
    -- l - lebel for then instructions
    -- l+1 - lebel for else instructions
    -- l+2 - end lebel (with next instructions)
    -- generating block with if condition
    cond_blocks <- generateBoolExprIRElems e ("lebel" ++ show l) 
                ("lebel" ++ show (l+1))
    (s''', (_, n'''), (_, l''', _), f''', v''', str''') <- get
    put (s''', ([], n'''), (cond_blocks, l''', 
        L ("lebel" ++ show l)), f''', v''', str''')
    -- generating block for then
    generateStmtIRElems stmt1
    (s', (elems', n'), (blocks', l', lebel1'), f', _, structs') <- get
    let b2 = (lebel1', elems',
            L ("lebel" ++ show (l+2)))
    put (s', ([], n'), (blocks' ++ [b2], l', 
        L ("lebel" ++ show (l + 1))), f', v''', structs')
    -- generating block for else
    generateStmtIRElems stmt2
    (s'', (elems'', n''), (blocks'', l'', lebel1''), 
        f'', _, str'') <- get
    let b3 = (lebel1'', elems'', NoJump)
    put (s'', ([], n''), (blocks'' ++ [b3], l'', 
        L ("lebel" ++ show (l + 2))), f'', v''', str'')
    return ()
generateStmtIRElems (Cond _ e stmt) = do
    (s, ir_e, (blocks, l, lebel1), f, v, structs) <- get
    put (s, ir_e, (blocks, l + 2, lebel1), f, v, structs)
    -- l - lebel for then instructions
    -- l+1 - end lebel (with next instructions)
    cond_blocks <- generateBoolExprIRElems e 
            ("lebel" ++ show l) ("lebel" ++ show (l+1))
    -- generating stmt
    (s', (_, n'), (_, l', _), f', v', structs') <- get
    put (s', ([], n'), (cond_blocks, l', 
        L ("lebel" ++ show l)), f', v', structs')
    generateStmtIRElems stmt
    -- generating block for statement's instructions
    (s'', (elems'', n''), (blocks'', l'', lebel1''), 
        f'', _, str'') <- get
    let b = (lebel1'', elems'', L ("lebel" ++ show (l+1)))
    put (s'', ([], n''), (blocks'' ++ [b], l'', 
        L ("lebel" ++ show (l+1))), f'', v', str'')
    return ()
generateStmtIRElems (While _ e stmt) =  do
    -- l - label for while body
    -- l+1 - label for while condition
    -- l+2 - end lebel (with next instructions)
    -- genereting block with condition and IR elems gen so far
    (s, (elems, n), (blocks, l, lebel1), f, v, structs) <- get
    let b1 = (lebel1, elems,
             L ("lebel" ++ show (l+1)))
    put (s, ([], n), (blocks ++ [b1], l + 3, 
        L ("lebel" ++ show l)), f, v, structs)
    -- generating block for while body
    generateStmtIRElems stmt
    (s', (elems', n'), (blocks', l', lebel1'), f', _, str') <- get
    let b2 = (lebel1', elems', NoJump)
    put (s', ([], n'), (blocks' ++ [b2], l', 
        L ("lebel" ++ show (l + 1))), f', v, str')
    -- generating blocks for while condidtion
    cond_blocks <- generateBoolExprIRElems e 
                ("lebel" ++ show l) ("lebel" ++ show (l+2))
    (s'', (_, n''), (_, l'', _), f'', v'', str'') <- get
    put (s'', ([], n''), (cond_blocks, l'', 
        L ("lebel" ++ show (l + 2))), f'', v'', str'')
    return ()
generateStmtIRElems (SExp _ e) = do
    _ <- generateExprIRElems e
    return ()
generateStmtIRElems (For pos typ iden arr stmt) = do
    -- making while from for
    let name = "_for_variable"
    let countDecl = Decl pos (BltinType Nothing (Int Nothing)) 
                    [Init pos (Ident name) (ELitInt pos 0)]
    let iterDecl = Decl pos typ [NoInit pos iden]
    let whileCond = ERel pos (EVar pos (Ident name)) 
                        (LTH pos) 
                        (EStructField pos arr (Ident "length"))
    let assignIter = Ass pos (EVar pos iden) 
                            (EArrAt pos arr (EVar pos (Ident name)))
    let incrCount = Incr pos (Ident name)
    let whileBody = BStmt pos (Block pos [assignIter, stmt, incrCount])
    let whileStmt = While pos whileCond whileBody
    let new_for = BStmt pos (Block pos [countDecl, iterDecl, whileStmt])
    generateStmtIRElems new_for


-- Function returns a list containing function arguments'
-- identificators
getArgsIdents :: [Arg ErrorPos] -> [Ident]
getArgsIdents [] = []
getArgsIdents ((Arg _ _ iden):rest) = 
    [iden] ++ (getArgsIdents rest)


-- Function creates IR blocks for given function
getBlocksFunDef :: TopDef ErrorPos -> IRStore ([Ident], [IRBlock])
getBlocksFunDef (FnDef _ _ iden args (Block _ stmts)) = do
    -- adding label name with function's name
    (s, (_, n), (_, l, _), funcs, _, structs) <- get
    put (s, ([], n), ([], l, L ("_" ++ (getName iden))), funcs, M.empty, structs)
    --addArgsToEnv 0 args
    addArgsToEnv args
    -- generating blocks
    generateBlockIRElems stmts
    -- adding final lebel
    (s', (elems', n'), (blocks', l', l1'), funcs', vars, structs') <- get
    let b = (l1', elems', VRetL)
    put (s', ([], n'), (blocks' ++ [b], l' + 1, 
        L ("lebel" ++ show l')), funcs', vars, structs')
    let args_idents = getArgsIdents args
    return (args_idents, blocks' ++ [b])
getBlocksFunDef (StructDef _ _ _) = undefined


-- Function runs creating IR representation for each proggram's element.
-- It returns list of IR blocks and map constaining strings' locations
getIRBlocks :: [TopDef ErrorPos] -> [([Ident],[IRBlock])] 
                -> IRStore ([([Ident],[IRBlock])], StringStore)
getIRBlocks [] blocks = do
    (s, _, (_, _, _), _, _, _) <- get
    return (blocks, s)
getIRBlocks (el:rest) blocks = do
    case el of 
        (FnDef _ _ _ _ _) -> do
            b <- getBlocksFunDef el
            getIRBlocks rest (blocks ++ [b])
        _ -> getIRBlocks rest blocks


-- Function starts creating IR representation in StateT monad
getIRRepresentation ::  Program ErrorPos -> IO ([([Ident],[IRBlock])], StringStore)
getIRRepresentation (Program _ prog) = do
    let (funcs_types, structs) = getFunctionsTypes M.empty M.empty prog
    res <- runExceptT (runStateT (getIRBlocks prog []) 
                (M.empty, ([], 0), ([], 1, L "lebel0"), 
                funcs_types, M.empty, structs))
    case res of
        Left err -> return ([], M.insert err (StrLoc 1) M.empty)
        Right ((blocks, strStore),_) ->
            return (blocks, strStore)

