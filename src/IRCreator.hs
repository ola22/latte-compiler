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
    VarReg String
    deriving Show

-- operations used in IR
data IROp = 
    IRAdd | IRSub | IRTimes | IRDiv | IRMod |
    IRLTH | IRLE | IRGTH | IRGE | IREQU | IRNE
    deriving Show

-- Elements of "code" used for creating IR
data IRElem = 
    IROp IRReg IROp IRReg IRReg | -- arithmetic op (res_reg, op, reg1, reg2)
    IRCall IRReg String [IRReg] | -- func call (res_reg, func_name, args_regs)
    IRVarToReg IRReg IRReg | -- assigning var (var_reg, val_reg)
    IRVarFromReg IRReg IRReg -- getting var's val to reg r (r, var_reg)
    deriving Show

-- data for storeing labels used in blocks
data Label = 
    L String | -- block ended by unconditional jump
    CondL IRReg String String | -- block ended by conditional jump
    RetL IRReg | -- block ended by return
    VRetL -- block ended by void return
    deriving Show

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

-- Store for creating IR representation
-- (string env, IR elems env, blocks env, funcs types, vars types and their new names)
type IRStore = StateT (StringStore, IRElemsStore, IRBlockStore, FuncTypMap, VarEnv) 
                        (ExceptT String IO)




-- TODO: 
-- consty, żeby sie odrazu wyliczały
-- te reversy, czy ja to wgl dobrze sklejam :()


-- Function returns type of given expression
getExprType :: Expr ErrorPos -> FuncTypMap -> VarEnv -> Type ErrorPos
getExprType e funcs vars = do
    let typ = getType e
    case typ of
        Just t -> t
        Nothing ->
            -- we know here that e is FAppl or Var
            case e of
                (EVar _ iden) -> do
                    case M.lookup (getName iden) vars of
                        Nothing -> undefined
                            --throwError "undeclared variable" -- debug, niegdy nie powinno mieć miejsca
                        Just (_, _, var_type) -> var_type
                (EApp _ iden _) -> do
                    case M.lookup iden funcs of
                        Nothing -> undefined
                            --throwError "undeclared function" -- debug, niegdy nie powinno mieć miejsca
                        Just func_type -> func_type
                _ -> undefined


-- Function adds given variable and its type to variables'
-- environment. If variable of given name was already declared 
-- it is renamed.
addVarToEnv :: Ident -> Type ErrorPos -> IRStore ()
addVarToEnv iden typ = do
    (str, ir, b, funcs, vars) <- get
    let v_n = getName iden
    case M.lookup v_n vars of
        -- variable declared for the first time
        -- we don't have to rename it
        Nothing -> do
            let vars' = M.insert v_n (v_n, 1, typ) vars
            put (str, ir, b, funcs, vars')
            return ()
        -- variable with this name already exists
        -- we change the name of new value
        Just (_, n, _) -> do
            let new_n = "newn" ++ v_n ++ show n
            let vars' = M.insert v_n (new_n, n+1, typ) vars
            put (str, ir, b, funcs, vars')
            return ()


--Function adds function's arguments from givem list
-- to variables' environment and renames them as arg1,...
-- data Arg a = Arg a (Type a) Ident
addArgsToEnv :: Integer -> [Arg ErrorPos] -> IRStore ()
addArgsToEnv _ [] = return ()
addArgsToEnv n ((Arg _ typ iden):rest) = do
    (str, ir, b, funcs, vars) <- get
    -- we know, that vars environment is empty rigth now
    let v_n = getName iden
    let new_n = "arg" ++ show n
    let vars' = M.insert v_n (new_n, 1, typ) vars
    put (str, ir, b, funcs, vars')
    addArgsToEnv (n+1) rest


-- Function returns current name of given variable
-- since it could have been renamed.
getCurVarName :: Ident -> IRStore (String)
getCurVarName iden = do
    (_, _, _, _, vars) <- get
    let v_n = getName iden
    case M.lookup v_n vars of
        -- variable should always be declared
        -- after semantic check
        Nothing -> throwError "undeclared variable" -- debug, niegdy nie powinno mieć miejsca
        -- getting current variable name
        Just (cur_n, _, _) -> return (cur_n)


-- Function returns mul operation operator
getMulIROp :: MulOp ErrorPos -> IROp
getMulIROp (Times _) = IRTimes
getMulIROp (Div _) = IRDiv
getMulIROp (Mod _) = IRMod


-- Function returns relop
getRelIROp :: RelOp ErrorPos -> IROp
getRelIROp (LTH _) = IRLTH
getRelIROp (LE _) = IRLE
getRelIROp (GTH _) = IRGTH
getRelIROp (GE _) = IRGE
getRelIROp (EQU _) = IREQU
getRelIROp (NE _) = IRNE


-- Function returns addition operator
getAddIROp :: AddOp ErrorPos -> IROp
getAddIROp (Plus _) = IRAdd
getAddIROp (Minus _) = IRSub


-- Function returns register for string and 
-- adds this string to StringStore
getStrIRReg :: String -> IRStore (IRReg)
getStrIRReg s = do
    (s_store, (elems , n), b, f, v) <- get
    case M.lookup s s_store of
        Nothing -> do
            let reg = StrLoc n
            put (M.insert s reg s_store, 
                (elems, n+1), b, f, v)
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
generateRelIRElems (ERel _ e1 op e2) = do
    e1_reg <- generateExprIRElems e1
    e2_reg <- generateExprIRElems e2
    -- tutsj chyba, że jak dwa consty to const -> nie no, tu juz chyba nie
    (s, (elems, n), b, funcs, vars) <- get
    let res_reg = Reg n
    case op of
        EQU _ -> do
            let typ = getExprType e1 funcs vars                              -- TODO to wywalic do jednej funkcji
            case typ of
                Int _ -> do
                    let new_el = IROp res_reg IREQU e1_reg e2_reg
                    put (s, (elems ++ [new_el], n + 1), b, funcs, vars)
                    return (res_reg)
                Bool _ -> do
                    let new_el = IROp res_reg IREQU e1_reg e2_reg
                    put (s, (elems ++ [new_el], n + 1), b, funcs, vars)
                    return (res_reg)
                Str _ -> do
                    let new_el = IRCall res_reg "strcmp" [e1_reg, e2_reg] 
                    put (s, (elems ++ [new_el], n + 1), b, funcs, vars)
                    return (res_reg)
                _ -> undefined
        NE _ -> do
            let typ = getExprType e1 funcs vars
            case typ of
                Int _ -> do
                    let new_el = IROp res_reg IRNE e1_reg e2_reg
                    put (s, (elems ++ [new_el], n + 1), b, funcs, vars)
                    return (res_reg)
                Bool _ -> do
                    let new_el = IROp res_reg IRNE e1_reg e2_reg
                    put (s, (elems ++ [new_el], n + 1), b, funcs, vars)
                    return (res_reg)
                Str _ -> do
                    let new_el = IRCall res_reg "strcmpn" [e1_reg, e2_reg] 
                    put (s, (elems ++ [new_el], n + 1), b, funcs, vars)
                    return (res_reg)
                _ -> undefined
        _ -> do
            let new_el = IROp res_reg (getRelIROp op) e1_reg e2_reg
            put (s, (elems ++ [new_el], n + 1), b, funcs, vars)
            return (res_reg)
generateRelIRElems _ = undefined


-- Function creates and adds to environment all
-- IR elements and blocks created for IR representation
-- from given expression.
generateExprIRElems :: Expr ErrorPos -> IRStore (IRReg)
generateExprIRElems (EVar _ iden) = do
    (s, (elems , n), b, funcs, vars) <- get
    let r = Reg n
    v_n <- getCurVarName iden
    let new_el = IRVarFromReg r (VarReg v_n)
    put (s, (elems ++ [new_el], n + 1), b, funcs, vars)
    return (r)
generateExprIRElems (ELitInt _ x) = return (Const x)
generateExprIRElems (ELitTrue _) = return (Const 1)
generateExprIRElems (ELitFalse _) = return (Const 0)
generateExprIRElems (EString _ s) = do
    reg <- getStrIRReg s
    return (reg)
generateExprIRElems (EApp _ iden exprs) = do       -- nie jestem pewna,cz y sama logika tego jest ok :/
    args_regs <- getArgsRegsAndIRElems exprs
    (s, (elems, n), b, funcs, vars) <- get
    let res_reg = Reg n
    v_n <- getCurVarName iden
    let el = IRCall res_reg v_n args_regs
    put (s, (elems ++ [el], n+1), b, funcs, vars)
    return (res_reg)
generateExprIRElems (Neg _ e) = do  -- do Integerow
    e_reg <- generateExprIRElems e
    (s, (elems, n), b, funcs, vars) <- get
    let res_reg = Reg n
    let new_el = IROp res_reg IRSub (Const 0) e_reg
    put (s, (elems ++ [new_el], n + 1), b, funcs, vars)
    return (res_reg)
generateExprIRElems (Not _ e) = do  -- do booli
    e_reg <- generateExprIRElems e
    (s, (elems, n), b, funcs, vars) <- get
    let res_reg = Reg n
    let new_el = IROp res_reg IRSub (Const 1) e_reg
    put (s, (elems ++ [new_el], n + 1), b, funcs, vars)
    return (res_reg)
generateExprIRElems (EMul _ e1 op e2) = do            -- TODO , ZE JAK DWA CONSTY TO CONST A NIE JEAKIES GOWNO
    e1_reg <- generateExprIRElems e1
    e2_reg <- generateExprIRElems e2
    -- tutsj chyba, że jak dwa consty to const
    (s, (elems, n), b, funcs, vars) <- get
    let res_reg = Reg n
    let new_el = IROp res_reg (getMulIROp op) e1_reg e2_reg
    put (s, (elems ++ [new_el], n + 1), b, funcs, vars)
    return (res_reg)
generateExprIRElems (EAdd _ e1 op e2) = do          -- TODO , ZE JAK DWA CONSTY TO CONST A NIE JEAKIES GOWNO
    e1_reg <- generateExprIRElems e1
    e2_reg <- generateExprIRElems e2
    -- tutsj chyba, że jak dwa consty to const
    (s, (elems, n), b, funcs, vars) <- get
    let res_reg = Reg n
    case op of
        Minus _ -> do
            let new_el = IROp res_reg IRSub e1_reg e2_reg
            put (s, (elems ++ [new_el], n + 1), b, funcs, vars)
            return (res_reg)
        Plus _ -> do
            let typ = getExprType e1 funcs vars
            case typ of
                Int _ -> do
                    let new_el = IROp res_reg IRAdd e1_reg e2_reg
                    put (s, (elems ++ [new_el], n + 1), b, funcs, vars)
                    return (res_reg)
                _ -> do
                    let new_el = IRCall res_reg "strconcat" [e1_reg, e2_reg] 
                    put (s, (elems ++ [new_el], n + 1), b, funcs, vars)
                    return (res_reg)
generateExprIRElems (ERel pos e1 op e2) = 
    generateRelIRElems (ERel pos e1 op e2)
generateExprIRElems (EAnd _ e1 e2) = do
    -- creating new IR blocks for making AND
    e1_reg <- generateExprIRElems e1
    (s, (elems, n), (blocks, l, lebel1), funcs, vars) <- get
    let name_reg = Reg n                                                                         -- TODO O CO TU CHODZI
    let res_reg = Reg (n + 1)
    let b1 = (lebel1, elems, 
            CondL e1_reg ("lebel" ++ show l) ("lebel" ++ show (l+1)))
    let b2 = (L ("lebel" ++ show (l+1)), 
            [IRVarToReg name_reg (Const 0)], L ("lebel" ++ show (l+2)))
    put (s, ([], n+2), (blocks ++ [b1] ++ [b2], l + 3, 
        L ("lebel" ++ show l)), funcs, vars)
    e2_reg <- generateExprIRElems e2
    (s', (elems', n'), (blocks', l', lebel1'), funcs', vars') <- get
    let b3 = (lebel1', elems' ++ [IRVarToReg name_reg e2_reg], 
            L ("lebel" ++ show (l+2)))
    put (s', ([], n'), (blocks' ++ [b3], l', 
        L ("lebel" ++ show (l+2))), funcs', vars')
    return (res_reg)
    --secondcondlebel l
    -- konownfalse l+1
    -- endlabel l+2
    -- varregister
    -- res_reg
generateExprIRElems (EOr _ e1 e2) = do
    -- creating new IR blocks for making OR
    e1_reg <- generateExprIRElems e1
    (s, (elems, n), (blocks, l, lebel1), funcs, vars) <- get
    let name_reg = Reg n
    let res_reg = Reg (n + 1)
    let b1 = (lebel1, elems, 
            CondL e1_reg ("lebel" ++ show (l+1)) ("lebel" ++ show l))
    let b2 = (L ("lebel" ++ show (l+1)), 
            [IRVarToReg name_reg (Const 1)], L ("lebel" ++ show (l+2)))
    put (s, ([], n+2), (blocks ++ [b1] ++ [b2], l + 3, 
        L ("lebel" ++ show l)), funcs, vars)
    e2_reg <- generateExprIRElems e2
    (s', (elems', n'), (blocks', l', lebel1'), funcs', vars') <- get
    let b3 = (lebel1', elems' ++ [IRVarToReg name_reg e2_reg], 
            L ("lebel" ++ show (l+2)))
    put (s', ([], n'), (blocks' ++ [b3], l', 
        L ("lebel" ++ show (l+2))), funcs', vars')
    return (res_reg)
    --secondcondlebel l
    -- konowntrue l+1
    -- endlabel l+2
    -- varregister
    -- res_reg


------------------------------------------------- GENERATING IR FOR STATEMENTS -------------------------------------------------------------------


-- TODO połaćzyć ta i ta niżej koniecznie, bo sa bez sensu
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
            (s, (elems , n), b, funcs, vars) <- get
            put (s, (elems ++ [el'], n), b, funcs, vars)
        Init _ iden e -> do
            addVarToEnv iden typ
            v_n <- getCurVarName iden
            e_reg <- generateExprIRElems e
            let el' = IRVarToReg (VarReg v_n) e_reg
            (s, (elems , n), b, funcs, vars) <- get
            put (s, (elems ++ [el'], n), b, funcs, vars)
    generateDeclIRElems typ rest                                         -- CZY TO SIE NAPEWNO WYWOLA?????????????????


-- Function genrates all necassary IR elements for given list
-- of items, which are strings
generateDeclIRElemsForStr :: [Item ErrorPos] -> IRStore ()
generateDeclIRElemsForStr [] = return ()
generateDeclIRElemsForStr (el:rest) = do
    case el of
        NoInit _ iden -> do
            addVarToEnv iden (Str Nothing)
            v_n <- getCurVarName iden
            str_reg <- getStrIRReg ""
            let el' = IRVarToReg (VarReg v_n) str_reg
            (s, (elems , n), b, f, v) <- get
            put (s, (elems ++ [el'], n), b, f, v)
        Init _ iden e -> do
            addVarToEnv iden (Str Nothing)
            v_n <- getCurVarName iden
            e_reg <- generateExprIRElems e
            let el' = IRVarToReg (VarReg v_n) e_reg
            (s, (elems , n), b, f, v) <- get
            put (s, (elems ++ [el'], n), b, f, v)
    generateDeclIRElemsForStr rest


-- Function generates IR elements for each od 
-- statements from given list
generateBlockIRElems :: [Stmt ErrorPos] -> IRStore ()
generateBlockIRElems [] = return ()
generateBlockIRElems (stmt:rest) = do
    generateStmtIRElems stmt
    generateBlockIRElems rest


-- Function creates and adds to environment all
-- IR elements and blocks created for IR representation
-- from given statement.
generateStmtIRElems :: Stmt ErrorPos -> IRStore ()
generateStmtIRElems (Empty _) = return ()
generateStmtIRElems (BStmt _ (Block _ stmts)) = do
    -- ending the IR block created so far
    (s, (elems, n), (blocks, l, lebel1), funcs, vars) <- get
    let b = (lebel1, elems, L ("lebel" ++ show l))
    put (s, ([], n), (blocks ++ [b], l + 1, 
        L ("lebel" ++ show l)), funcs, vars)
    -- starting new IR block for block of stmts
    generateBlockIRElems stmts
    (s', (elems', n'), (blocks', l', lebel1'), funcs', _) <- get
    let b2 = (lebel1', elems', L ("lebel" ++ show l'))      -- nie mam pojęcia jka to zrobić :< czy tu nazw nie pozmieniac
    -- restoring old variables env after exiting inner block
    put (s', ([], n'), (blocks' ++ [b2], l' + 1, 
        L ("lebel" ++ show l')), funcs', vars)
    return ()
    -- nextlebel' = l'
generateStmtIRElems (Decl _ typ items) = 
    case typ of
        Str _ -> generateDeclIRElemsForStr items
        typ' -> generateDeclIRElems typ' items
generateStmtIRElems (Ass _ iden e) = do
    e_reg <- generateExprIRElems e
    (s, (elems, n), b, funcs, vars) <- get
    v_n <- getCurVarName iden
    let new_el = IRVarToReg (VarReg v_n) e_reg            -- czy tu wgl moga takie byc?????????
    put (s, (elems ++ [new_el], n), b, funcs, vars)
    return ()
generateStmtIRElems (Incr _ iden) = do
    (s, (elems, n), b, funcs, vars) <- get
    v_n <- getCurVarName iden
    let var_reg = VarReg v_n
    let el1 = IRVarFromReg (Reg n) var_reg
    let el2 = IROp (Reg (n+1)) IRAdd (Reg n) (Const 1)
    let el3 = IRVarToReg var_reg (Reg (n+1))
    put (s, (elems ++ [el1] ++ [el2] ++ [el3], n + 2), b, funcs, vars)
    return ()
generateStmtIRElems (Decr _ iden) = do
    (s, (elems, n), b, funcs, vars) <- get
    v_n <- getCurVarName iden
    let var_reg = VarReg v_n -- register with given variable
    let el1 = IRVarFromReg (Reg n) var_reg
    let el2 = IROp (Reg (n+1)) IRSub (Reg n) (Const 1) 
    let el3 = IRVarToReg var_reg (Reg (n+1))
    put (s, (elems ++ [el1] ++ [el2] ++ [el3], n + 2), b, funcs, vars)
    return ()
generateStmtIRElems (Ret _ e) = do
    e_reg <- generateExprIRElems e
    (s, (elems, n), (blocks, l, lebel1), funcs, vars) <- get
    let b = (lebel1, elems, RetL e_reg)
    put (s, ([], n), (blocks ++ [b], l + 1, 
        L ("lebel" ++ show l)), funcs, vars)
    return ()
generateStmtIRElems (VRet _) = do
    (s, (elems, n), (blocks, l, lebel1), funcs, vars) <- get
    let b = (lebel1, elems, VRetL)
    put (s, ([], n), (blocks ++ [b], l + 1, 
        L ("lebel" ++ show l)), funcs, vars)
    return ()
generateStmtIRElems (CondElse _ e stmt1 stmt2) = do
    -- generating block with if condition
    e_reg <- generateExprIRElems e
    (s, (elems, n), (blocks, l, lebel1), f, v) <- get
    let b = (lebel1, elems, CondL e_reg ("lebel" ++ show l) 
            ("lebel" ++ show (l+1)))
    put (s, ([], n), (blocks ++ [b], l + 3, 
        L ("lebel" ++ show l)), f, v)
    -- generating block for then
    generateStmtIRElems stmt1
    (s', (elems', n'), (blocks', l', lebel1'), f', v') <- get
    let b2 = (lebel1', elems',
            L ("lebel" ++ show (l+2)))
    put (s', ([], n'), (blocks' ++ [b2], l', 
        L ("lebel" ++ show (l + 1))), f', v')
    -- generating block for else
    generateStmtIRElems stmt2
    (s'', (elems'', n''), (blocks'', l'', lebel1''), f'', v'') <- get
    let b3 = (lebel1'', elems'',
         L ("lebel" ++ show (l+2)))
    put (s'', ([], n''), (blocks'' ++ [b3], l'', 
        L ("lebel" ++ show (l + 2))), f'', v'')
    return ()
    -- then lebel l
    -- elselebel l+1
    --endlebel l+2
generateStmtIRElems (Cond _ e stmt) = do
    -- generating block with if condition
    e_reg <- generateExprIRElems e
    (s, (elems, n), (blocks, l, lebel1), f, v) <- get
    let b = (lebel1, elems, 
            CondL e_reg ("lebel" ++ show l) ("lebel" ++ show (l+1)))
    put (s, ([], n), (blocks ++ [b], l + 2, 
        L ("lebel" ++ show l)), f, v)
    -- generating block for then
    generateStmtIRElems stmt
    (s', (elems', n'), (blocks', l', lebel1'), f', v') <- get
    let b2 = (lebel1', elems', 
            L ("lebel" ++ show (l+1)))
    put (s', ([], n'), (blocks' ++ [b2], l', 
        L ("lebel" ++ show (l + 1))), f', v')
    -- then lebel l
    -- endlebel - l+1
    return ()
generateStmtIRElems (While _ e stmt) =  do
    -- genereting block with condition and IR elems gen so far
    (s, (elems, n), (blocks, l, lebel1), f, v) <- get
    let b1 = (lebel1, elems,
             L ("lebel" ++ show (l+1)))
    put (s, ([], n), (blocks ++ [b1], l + 3, L ("lebel" ++ show l)), f, v)
    -- generating block for while body
    generateStmtIRElems stmt
    (s', (elems', n'), (blocks', l', lebel1'), f', v') <- get
    let b2 = (lebel1', elems', 
            L ("lebel" ++ show (l+1)))
    put (s', ([], n'), (blocks' ++ [b2], l', 
        L ("lebel" ++ show (l + 1))), f', v')
    -- generating block for while condidtion
    e_reg <- generateExprIRElems e
    (s'', (elems'', n''), (blocks'', l'', lebel1''), f'', v'') <- get
    let b3 = (lebel1'', elems'', 
            CondL e_reg ("lebel" ++ show l) ("lebel" ++ show (l+2)))
    put (s'', ([], n''), (blocks'' ++ [b3], l'', 
        L ("lebel" ++ show (l + 2))), f'', v'')
    return ()
    -- bodylabel l
    -- condlabel l+1
    -- endlabel l+2
generateStmtIRElems (SExp _ e) = do
    _ <- generateExprIRElems e
    return ()


-- Function creates IR blocks for given function
getBlocksFunDef :: TopDef ErrorPos -> IRStore ()
getBlocksFunDef (FnDef _ _ iden args (Block _ stmts)) = do
    -- adding label name with function's name
    (s, (_, n), (blocks, l, _), funcs, _) <- get
    put (s, ([], n), (blocks, l, L (getName iden)), funcs, M.empty)
    addArgsToEnv 0 args
    -- generating blocks
    generateBlockIRElems stmts
    -- adding final lebel
    (s', (elems', n'), (blocks', l', l1'), funcs', vars) <- get
    let b = (l1', elems', VRetL)
    put (s', ([], n'), (blocks' ++ [b], l' + 1, L ("lebel" ++ show l')), funcs', vars)


-- Function runs creating IR representation for each proggram's element.
-- It returns list of IR blocks and map constaining strings' locations
getIRBlocks :: [TopDef ErrorPos] -> IRStore ([IRBlock], StringStore)
getIRBlocks [] = do
    (s, _, (blocks, _, _), _, _) <- get
    return (blocks, s)
getIRBlocks (el:rest) = do
    getBlocksFunDef el
    getIRBlocks rest


-- Function starts creating IR representation in StateT monad
getIRRepresentation ::  Program ErrorPos -> IO ([IRBlock], StringStore)
getIRRepresentation (Program _ prog) = do
    let funcs_types = getFunctionsTypes M.empty prog
    res <- runExceptT (runStateT (getIRBlocks prog) 
                (M.empty, ([], 0), ([], 1, L "lebel0"), funcs_types, M.empty))
    case res of
        Left _ -> return ([], M.empty)
        Right ((blocks, strStore),_) ->
            return (blocks, strStore)

