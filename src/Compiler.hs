{-# Options -Wall -Wname-shadowing #-}

module Compiler where


import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Except

import Latte.Parsing.AbsLatte
import Common.ErrorPositions
import Common.Common





data IRReg = 
    Const Integer | -- just constants
    Reg Integer | -- normal registers
    StrLoc Integer | -- string locations
    VarReg String
    deriving Show

data IROp = 
    IRAdd |
    IRSub |
    IRTimes |
    IRDiv |
    IRMod |
    IRLTH |
    IRLE |
    IRGTH |
    IRGE |
    IREQU |
    IRNE
    deriving Show

-- Elements of "code" used for creating intermediate representation
data IRElem = 
    IROp IRReg IROp IRReg IRReg | -- arithmetic op (res_reg, op, reg1, reg2)
    IRCall IRReg String [IRReg] | -- func call (res_reg, func_name, args_regs)
    IRVarToReg IRReg IRReg | -- assignvar nwm czemu tylko jeden rejestr
    IRVarFromReg IRReg IRReg -- getvar wez z rejestru wartosc jakiejs zmiennej (res_reg, reg_zmeinnej, zmienna)
    deriving Show
    --IRVarToReg IRReg IRReg String |  -- zapisz do rejestru x wartosc zmiennej y (reg, var_val)  TODO czy ja tu wgl chce trzymac nazwe zmiennej, czy sama wartość?/???
    --IRVarToRegAgain IRReg Integer String |
    --IRValToReg IRReg Integer | -- zapisz do rejestru x wartosc 3 (reg, val)
    --IRAlloca IRReg  --                TODO CZY POTRZEBUJE CZEGOS NA ALLOCA????????????????????????????

-- data for storeing labels used in blocks
data Label = 
    L String | -- block ended by unconditional jump
    CondL IRReg String String | -- block ended by conditional jump
    RetL IRReg | -- block ended by return
    VRetL -- block ended by void return
    deriving Show


type StringStore = M.Map String IRReg -- string -> rejestr, wskaźnik???

type IRBlock =  (Label, [IRElem], Label)
type IRBlockStore = ([IRBlock], Integer, Integer) -- bloki, label wolny, label poczatkowy aktualnego bloku
type IRElemsStore = ([IRElem], Integer) -- akt blok, rejestr wolny

-- environment used for renaming variables, 
-- which has the same name
-- old_name -> (seed, new_name)
type VarEnv = M.Map String (Integer, String)   -- TROCHE NWM, JAK TO ZROBIC????

type IRStore = StateT (StringStore, IRElemsStore, IRBlockStore) (ExceptT String IO) -- (stringi, akt blok, bloki, label wolny, rejestr wolny)




-- TODO: (dużo rzeczy takich super ważnych pomijam pisząć to dlatego strasznie panikuje :/)
-- przenazywanie zmiennych
-- typy funkcji -> dwa dodatkowe środowiska
-- consty, żeby sie odrazu wyliczały
-- te reversy, czy ja to wgl dobrze sklejam :()
-- wywalic to lebel wszedzie, bo nazwy funkcji slabo, albo dodac ten generator tych ziomali może -> albo zamiast nazw funkcji bedzie w tym kodzie lebelnazwafunkcji
-- dodawanie argumentow funkcji -> 
    -- zeby sie kompilowalo wywolywane z maina czy cos






-- Function returns type of given expression                        TODO TYP JAEŚLI WYWOŁANIE FUNKCJI ZALBO ZMIENNA!!!!!!!!!!!!!!!!! -> całe środowisko trzeba dodać :<
getExprType :: Expr ErrorPos -> IRStore (Type ErrorPos)
getExprType e = do
    let typ = getType e
    case typ of
        Just t -> return (t)
        Nothing -> return (Int Nothing)---szukamy dalej



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
    (s_store, (elems , n), b) <- get
    case M.lookup s s_store of
        Nothing -> do
            let reg = StrLoc n
            put (M.insert s reg s_store, (elems, n+1), b)
            return (reg)
        Just reg -> return (reg)



-- Function returns list of registers of function's arguments
-- and adds theirs IR elements to environment
getArgsRegsAndIRElems :: [Expr ErrorPos] -> IRStore ([IRReg])
getArgsRegsAndIRElems [] = return ([])
getArgsRegsAndIRElems (e:[]) = do
    reg <- generateExprIRElems e
    return ([reg]) -- zwraca liste z samego siebie
getArgsRegsAndIRElems (e:rest) = do
    -- najpierw licze siebie bo swoje quadsy chce najpierw
    reg <- generateExprIRElems e
    -- potem licze reszte
    rest_regs <- getArgsRegsAndIRElems rest
    -- i skelejam liste wynikowa, ale ich quadsy sa w dobrej kolejnosci :>
    return ([reg] ++ rest_regs) 

-- zaczynamy z label 0, rejestr 0, lista pusta


generateRelIRElems :: Expr ErrorPos -> IRStore (IRReg)
generateRelIRElems (ERel _ e1 op e2) = do
    e1_reg <- generateExprIRElems e1
    e2_reg <- generateExprIRElems e2
    -- tutsj chyba, że jak dwa consty to const
    (s, (elems, n), b) <- get
    let res_reg = Reg n
    case op of
        EQU _ -> do
            typ <- getExprType e1                                -- TODO to wywalic do jednej funkcji
            case typ of
                Int _ -> do
                    let new_el = IROp res_reg IREQU e1_reg e2_reg
                    put (s, (elems ++ [new_el], n + 1), b)
                    return (res_reg)
                Bool _ -> do
                    let new_el = IROp res_reg IREQU e1_reg e2_reg
                    put (s, (elems ++ [new_el], n + 1), b)
                    return (res_reg)
                Str _ -> do
                    let new_el = IRCall res_reg "strcmp" [e1_reg, e2_reg] 
                    put (s, (elems ++ [new_el], n + 1), b)
                    return (res_reg)
                _ -> undefined
        NE _ -> do
            typ <- getExprType e1 
            case typ of
                Int _ -> do
                    let new_el = IROp res_reg IRNE e1_reg e2_reg
                    put (s, (elems ++ [new_el], n + 1), b)
                    return (res_reg)
                Bool _ -> do
                    let new_el = IROp res_reg IRNE e1_reg e2_reg
                    put (s, (elems ++ [new_el], n + 1), b)
                    return (res_reg)
                Str _ -> do
                    let new_el = IRCall res_reg "strcmpn" [e1_reg, e2_reg] 
                    put (s, (elems ++ [new_el], n + 1), b)
                    return (res_reg)
                _ -> undefined
        _ -> do
            let new_el = IROp res_reg (getRelIROp op) e1_reg e2_reg
            put (s, (elems ++ [new_el], n + 1), b)
            return (res_reg)
generateRelIRElems _ = undefined



generateExprIRElems :: Expr ErrorPos -> IRStore (IRReg)
--generateExprIRElems e = return (1)
generateExprIRElems (EVar _ iden) = do
    (s, (elems , n), b) <- get
    let r = Reg n
    let new_el = IRVarFromReg r (VarReg (getName iden))
    put (s, (elems ++ [new_el], n + 1), b)
    return (r)
generateExprIRElems (ELitInt _ x) = return (Const x)
generateExprIRElems (ELitTrue _) = return (Const 1)
generateExprIRElems (ELitFalse _) = return (Const 0)
generateExprIRElems (EString _ s) = do
    reg <- getStrIRReg s
    return (reg)
generateExprIRElems (EApp _ iden exprs) = do       -- nie jestem pewna,cz y sama logika tego jest ok :/
    args_regs <- getArgsRegsAndIRElems exprs
    (s, (elems, n), b) <- get
    let res_reg = Reg n
    let el = IRCall res_reg (getName iden) args_regs
    put (s, (elems ++ [el], n+1), b)
    return (res_reg)
generateExprIRElems (Neg _ e) = do  -- do Integerow
    e_reg <- generateExprIRElems e
    (s, (elems, n), b) <- get
    let res_reg = Reg n
    let new_el = IROp res_reg IRSub (Const 0) e_reg
    put (s, (elems ++ [new_el], n + 1), b)
    return (res_reg)
generateExprIRElems (Not _ e) = do  -- do booli
    e_reg <- generateExprIRElems e
    (s, (elems, n), b) <- get
    let res_reg = Reg n
    let new_el = IROp res_reg IRSub (Const 1) e_reg
    put (s, (elems ++ [new_el], n + 1), b)
    return (res_reg)
generateExprIRElems (EMul _ e1 op e2) = do            -- TODO , ZE JAK DWA CONSTY TO CONST A NIE JEAKIES GOWNO
    e1_reg <- generateExprIRElems e1
    e2_reg <- generateExprIRElems e2
    -- tutsj chyba, że jak dwa consty to const
    (s, (elems, n), b) <- get
    let res_reg = Reg n
    let new_el = IROp res_reg (getMulIROp op) e1_reg e2_reg
    put (s, (elems ++ [new_el], n + 1), b)
    return (res_reg)
generateExprIRElems (EAdd _ e1 op e2) = do          -- TODO , ZE JAK DWA CONSTY TO CONST A NIE JEAKIES GOWNO
    e1_reg <- generateExprIRElems e1
    e2_reg <- generateExprIRElems e2
    -- tutsj chyba, że jak dwa consty to const
    (s, (elems, n), b) <- get
    let res_reg = Reg n
    case op of
        Minus _ -> do
            let new_el = IROp res_reg IRSub e1_reg e2_reg
            put (s, (elems ++ [new_el], n + 1), b)
            return (res_reg)
        Plus _ -> do
            typ <- getExprType e1 
            case typ of
                Int _ -> do
                    let new_el = IROp res_reg IRAdd e1_reg e2_reg
                    put (s, (elems ++ [new_el], n + 1), b)
                    return (res_reg)
                _ -> do
                    let new_el = IRCall res_reg "strAdd" [e1_reg, e2_reg] 
                    put (s, (elems ++ [new_el], n + 1), b)
                    return (res_reg)
generateExprIRElems (ERel pos e1 op e2) = generateRelIRElems (ERel pos e1 op e2)
generateExprIRElems (EAnd _ e1 e2) = do
    e1_reg <- generateExprIRElems e1
    (s, (elems, n), (blocks, l, lebel1)) <- get
    let name_reg = Reg n
    let res_reg = Reg (n + 1)
    let b1 = (L ("lebel" ++ show lebel1), elems, 
            CondL e1_reg ("lebel" ++ show l) ("lebel" ++ show (l+1)))
    let b2 = (L ("lebel" ++ show (l+1)), 
            [IRVarToReg name_reg (Const 0)], L ("lebel" ++ show (l+2)))
    put (s, ([], n+2), (blocks ++ [b1] ++ [b2], l + 3, l))

    e2_reg <- generateExprIRElems e2
    (s', (elems', n'), (blocks', l', lebel1')) <- get
    let b3 = (L ("lebel" ++ show lebel1'), 
            elems' ++ [IRVarToReg name_reg e2_reg], L ("lebel" ++ show (l+2)))
    put (s', ([], n'), (blocks' ++ [b3], l', l + 2))
    return (res_reg)
    --secondcondlebel l
    -- konownfalse l+1
    -- endlabel l+2
    -- varregister
    -- res_reg
generateExprIRElems (EOr _ e1 e2) = do
    e1_reg <- generateExprIRElems e1
    (s, (elems, n), (blocks, l, lebel1)) <- get
    let name_reg = Reg n
    let res_reg = Reg (n + 1)
    let b1 = (L ("lebel" ++ show lebel1), elems, 
            CondL e1_reg ("lebel" ++ show (l+1)) ("lebel" ++ show l))
    let b2 = (L ("lebel" ++ show (l+1)), 
            [IRVarToReg name_reg (Const 1)], L ("lebel" ++ show (l+2)))
    put (s, ([], n+2), (blocks ++ [b1] ++ [b2], l + 3, l))

    e2_reg <- generateExprIRElems e2
    (s', (elems', n'), (blocks', l', lebel1')) <- get
    let b3 = (L ("lebel" ++ show lebel1'), 
            elems' ++ [IRVarToReg name_reg e2_reg], L ("lebel" ++ show (l+2)))
    put (s', ([], n'), (blocks' ++ [b3], l', l + 2))
    return (res_reg)
    --secondcondlebel l
    -- konowntrue l+1
    -- endlabel l+2
    -- varregister
    -- res_reg



--data Item a = NoInit a Ident | Init a Ident (Expr a)
generateDeclIRElems :: [Item ErrorPos] -> IRStore ()                     -- TODO OSOBNA TAKA DLA STRINGOW
generateDeclIRElems [] = return ()
generateDeclIRElems (el:rest) = do
    case el of
        NoInit _ iden -> do
            let el' = IRVarToReg (VarReg (getName iden)) (Const 0)
            (s, (elems , n), b) <- get
            put (s, (elems ++ [el'], n), b)
        Init _ iden e -> do
            e_reg <- generateExprIRElems e
            let el' = IRVarToReg (VarReg (getName iden)) e_reg
            (s, (elems , n), b) <- get
            put (s, (elems ++ [el'], n), b)
    generateDeclIRElems rest                                         -- CZY TO SIE NAPEWNO WYWOLA?????????????????
    {-case el of 
        NoInit _ iden -> do
            (s, (elems , n), b) <- get
            let new_el = IRAlloca (Reg n)
            let new_el2 = IRVarToReg (Reg n) (Const 0) (getName iden)
            put (s, (elems ++ [new_el] ++ [new_el2], n + 1), b)
            generateDeclIRElems rest
        Init _ iden e -> do
            e_reg <- generateExprIRElems e
            (s, (elems , n), b) <- get
            let new_el = IRAlloca (Reg n)
            let new_el2 = IRVarToReg (Reg n) e_reg (getName iden)             -- czy tu wgl moga takie byc?????????
            put (s, (elems ++ [new_el] ++ [new_el2], n + 1), b)
            generateDeclIRElems rest-}


-- TO W SUMIE LEPEIEJ POŁĄCZYĆ Z TAMTA POPRZEDNIA   
generateDeclIRElemsForStr :: [Item ErrorPos] -> IRStore ()                     -- TODO OSOBNA TAKA DLA STRINGOW
generateDeclIRElemsForStr [] = return ()
generateDeclIRElemsForStr (el:rest) = do
    case el of
        NoInit _ iden -> do
            str_reg <- getStrIRReg ""
            let el' = IRVarToReg (VarReg (getName iden)) str_reg
            (s, (elems , n), b) <- get
            put (s, (elems ++ [el'], n), b)
        Init _ iden e -> do
            e_reg <- generateExprIRElems e
            let el' = IRVarToReg (VarReg (getName iden)) e_reg
            (s, (elems , n), b) <- get
            put (s, (elems ++ [el'], n), b)
    generateDeclIRElemsForStr rest


-- Function generates IR elements for each od 
-- statements from given list
generateBlockIRElems :: [Stmt ErrorPos] -> IRStore ()
generateBlockIRElems [] = return ()
generateBlockIRElems (stmt:rest) = do
    generateStmtIRElems stmt
    generateBlockIRElems rest




generateStmtIRElems :: Stmt ErrorPos -> IRStore ()
generateStmtIRElems (Empty _) = return ()
generateStmtIRElems (BStmt _ (Block _ stmts)) = do
    (s, (elems, n), (blocks, l, lebel1)) <- get
    let b = (L ("lebel" ++ show lebel1), elems, L ("lebel" ++ show l))
    put (s, ([], n), (blocks ++ [b], l + 1, l))

    generateBlockIRElems stmts
    (s', (elems', n'), (blocks', l', lebel1')) <- get
    -- nextlebel' = l'
    let b2 = (L ("lebel" ++ show lebel1'), elems', L ("lebel" ++ show l'))      -- nie mam pojęcia jka to zrobić :< czy tu nazw nie pozmieniac
    put (s', ([], n'), (blocks' ++ [b2], l' + 1, l'))
    return ()
generateStmtIRElems (Decl _ typ items) = 
    case typ of
        Str _ -> generateDeclIRElemsForStr items
        _ -> generateDeclIRElems items
generateStmtIRElems (Ass _ iden e) = do
    e_reg <- generateExprIRElems e
    (s, (elems, n), b) <- get
    let new_el = IRVarToReg (VarReg (getName iden)) e_reg            -- czy tu wgl moga takie byc?????????
    put (s, (elems ++ [new_el], n), b)
    return ()
generateStmtIRElems (Incr _ iden) = do
    (s, (elems, n), b) <- get
    let var_reg = (VarReg (getName iden))
    -- addr (VarReg (getName iden))
    -- tmpname reg n
    -- tmpname' reg n+1
    let el1 = IRVarFromReg (Reg n) var_reg
    let el2 = IROp (Reg (n+1)) IRAdd (Reg n) (Const 1)
    let el3 = IRVarToReg var_reg (Reg (n+1))
    put (s, (elems ++ [el1] ++ [el2] ++ [el3], n + 2), b)
    return ()
generateStmtIRElems (Decr _ iden) = do
    (s, (elems, n), b) <- get
    let var_reg = (VarReg (getName iden))
    let el1 = IRVarFromReg (Reg n) var_reg
    let el2 = IROp (Reg (n+1)) IRSub (Reg n) (Const 1)
    let el3 = IRVarToReg var_reg (Reg (n+1))
    put (s, (elems ++ [el1] ++ [el2] ++ [el3], n + 2), b)
    return ()
generateStmtIRElems (Ret _ e) = do
    e_reg <- generateExprIRElems e
    (s, (elems, n), (blocks, l, lebel1)) <- get
    let b = (L ("lebel" ++ show lebel1), elems, RetL e_reg)
    put (s, ([], n), (blocks ++ [b], l + 1, l))
    return ()
generateStmtIRElems (VRet _) = do
    (s, (elems, n), (blocks, l, lebel1)) <- get
    let b = (L ("lebel" ++ show lebel1), elems, VRetL)
    put (s, ([], n), (blocks ++ [b], l + 1, l))
    return ()
generateStmtIRElems (CondElse _ e stmt1 stmt2) = do   -- TERAZ cond, condelse i while i wreszcie koniec tego, TU SKONCZYLAM
    e_reg <- generateExprIRElems e
    (s, (elems, n), (blocks, l, lebel1)) <- get
    let b = (L ("lebel" ++ show lebel1), elems, CondL e_reg ("lebel" ++ show l) ("lebel" ++ show (l+1)))
    put (s, ([], n), (blocks ++ [b], l + 3, l))
    -- then lebel l
    generateStmtIRElems stmt1
    (s', (elems', n'), (blocks', l', lebel1')) <- get
    let b2 = (L ("lebel" ++ show lebel1'), elems', L ("lebel" ++ show (l+2)))
    put (s', ([], n'), (blocks' ++ [b2], l', l + 1))
    -- elselebel l+1
    generateStmtIRElems stmt2
    (s'', (elems'', n''), (blocks'', l'', lebel1'')) <- get
    let b3 = (L ("lebel" ++ show lebel1''), elems'', L ("lebel" ++ show (l+2)))
    put (s'', ([], n''), (blocks'' ++ [b3], l'', l + 2))
    return ()
    --endlebel l+2
generateStmtIRElems (Cond _ e stmt) = do
    e_reg <- generateExprIRElems e
    (s, (elems, n), (blocks, l, lebel1)) <- get
    let b = (L ("lebel" ++ show lebel1), elems, CondL e_reg ("lebel" ++ show l) ("lebel" ++ show (l+1)))
    put (s, ([], n), (blocks ++ [b], l + 2, l))
    -- then lebel l
    generateStmtIRElems stmt
    (s', (elems', n'), (blocks', l', lebel1')) <- get
    let b2 = (L ("lebel" ++ show lebel1'), elems', L ("lebel" ++ show (l+1)))
    put (s', ([], n'), (blocks' ++ [b2], l', l + 1))
    -- endlebel - l+1
    return ()
generateStmtIRElems (While _ e stmt) =  do
    (s, (elems, n), (blocks, l, lebel1)) <- get
    -- bodylabel l
    -- condlabel l+1
    -- endlabel l+2
    let b1 = (L ("lebel" ++ show lebel1), elems, L ("lebel" ++ show (l+1)))
    put (s, ([], n), (blocks ++ [b1], l + 3, l))

    generateStmtIRElems stmt
    (s', (elems', n'), (blocks', l', lebel1')) <- get
    let b2 = (L ("lebel" ++ show lebel1'), elems', L ("lebel" ++ show (l+1)))
    put (s', ([], n'), (blocks' ++ [b2], l', l + 1))

    e_reg <- generateExprIRElems e
    (s'', (elems'', n''), (blocks'', l'', lebel1'')) <- get
    let b3 = (L ("lebel" ++ show lebel1''), elems'', CondL e_reg ("lebel" ++ show l) ("lebel" ++ show (l+2)))
    put (s'', ([], n''), (blocks'' ++ [b3], l'', l + 2))
    return ()
generateStmtIRElems (SExp _ e) = do
    _ <- generateExprIRElems e
    return ()

{-

cond' <- generatequads cond - osobne dla exp i stmt -> ten kod jest w jakims rejestrze cond' rejestr w ktorym jestwynik (dla rejestrow bedzie to zwracac)
thenLabel <- getLabel - produkuje jakakolwiek nowa etykietke, quats o powstała nowa etykietka, 
elseLabel <- getLabel
endLabel <- getLabel - musza wiedziec dokad skoczysz po wykonaniu
prIntegerjumpa createJump cond' thenLabel elseLabel - czsem skaczemy do thenlebel czasem elselebel, -> musimy zapisac w jakis sposob, ze to jest koniec obecnego bloku conditionaljump
prIntegerpodwojnegolebela tenlabel/elselebel
prIntegerLabel thenLabel - prInteger label
generatequads stmt1
createJump endLabel
prIntegerLabel elseLabel
generateQuads stmt2
createJump endLabel
prIntegerLabel endLabel

data Stmt a
    = Empty a 
    | BStmt a (Block a)
    | Decl a (Type a) [Item a]
    | Ass a Ident (Expr a)
    | Incr a Ident
    | Decr a Ident
    | Ret a (Expr a)
    | VRet a
    | Cond a (Expr a) (Stmt a)
    | CondElse a (Expr a) (Stmt a) (Stmt a)
    | While a (Expr a) (Stmt a)
    | SExp a (Expr a)
  deriving (Eq, Ord, Show, Read)

generateStmtIRElems (Empty _) = (False, Nothing)
generateStmtIRElems (Decl _ _ _) = (False, Nothing)
generateStmtIRElems (Ass _ _ _) = (False, Nothing)
generateStmtIRElems (Incr _ _) = (False, Nothing)
generateStmtIRElems (Decr _ _) = (False, Nothing)
generateStmtIRElems (Ret _ _) = (True, Nothing) -- quadsy expra, koniec bloku
generateStmtIRElems (VRet _) = (True, Nothing) -- nie ma quadsow, koniec bloku
generateStmtIRElems (CondElse pos e stmt1 stmt2) = let  -- to tez mniej wiecej
generateStmtIRElems (Cond pos e stmt) =    -- to mam rozpisane

generateStmtIRElems (SExp _ e) = 



generateStmtIRElems (BStmt _ (Block _ stmts)) =         TODO

generateStmtIRElems (While _ e _) =   -- tego nie mam pojecia


-}


getBlocksFunDef :: TopDef ErrorPos -> IRStore ()
getBlocksFunDef (FnDef _ _ iden _ (Block _ stmts)) = do
    -- tu jeszcze pobrac, żeby dodac, że zaczynamy etykiete z nazwa funkcji
    (s, (_, n), (blocks, _, l1)) <- get
    --put (s, [], (blocks, l, (getName iden)))   -- TODO
    put (s, ([], n), (blocks, l1, l1))    -- TODO
    generateBlockIRElems stmts
    (s', (elems', n'), (blocks', l', l1')) <- get
    let b = (L ("lebel" ++ show l1'), elems', VRetL)
    put (s', ([], n'), (blocks' ++ [b], l' + 1, l'))


getIRBlocks :: [TopDef ErrorPos] -> IRStore ([IRBlock], StringStore)
getIRBlocks [] = do
    (s, _, (blocks, _, _)) <- get
    return (blocks, s)
getIRBlocks (el:rest) = do                       -- TODO
    getBlocksFunDef el
    getIRBlocks rest


-- Function starts compilatipn in StateT monad
compileProgram ::  Program ErrorPos -> IO ([IRBlock], StringStore)
compileProgram (Program _ prog) = do
    res <- runExceptT (runStateT (getIRBlocks prog) 
                (M.empty, ([], 0), ([], 1, 0)))
    case res of
        Left _ -> return ([], M.empty)
        Right ((blocks, strStore),_) ->
            return (blocks, strStore)



{-
type StringStore = M.Map String IRReg -- string -> rejestr, wskaźnik???

type IRBlock =  (Label, [IRElem], Label)
type IRBlockStore = ([IRBlock], Integer, Integer) -- bloki, label wolny, label poczatkowy aktualnego bloku
type IRElemsStore = ([IRElem], Integer) -- akt blok, rejestr wolny

-- environment used for renaming variables, 
-- which has the same name
-- old_name -> (seed, new_name)
type VarEnv = M.Map String (Integer, String)   -- TROCHE NWM, JAK TO ZROBIC????

type IRStore = StateT (StringStore, IRElemsStore, IRBlockStore) (ExceptT String IO) -- (stringi, akt blok, bloki, label wolny, rejestr wolny)

-}

{-

JAK NAPISAC BACKEND???

produkujemy quadsy
spłaszczamy strukture

podobny do llvma -> nie musimy trzymać typów -> 

jak masz llvma to masz rzeczy postaci
<destination> = <op> <source1> <source2>  -> operacje arytmetyczne

expressions -> podobnie jak llvm w małym kompilatorze
struktura -> trzymać czwórki jakieś

SSA -> postać w której trzymamy llvma
static single assignment -> każdy rejestr pojawia się raz
chcemy to sprowadzić do ssa

nie ma procentu -> każda nazwa po lewej
wszystkie rzeczy 64bitowe

negacja 
na incie 0 - Integer
bool 0/1 1 - bool

<op> -> moze byc string


data Expr a
    = EVar a Ident
    | ELitInteger a Integereger
    | ELitTrue a
    | ELitFalse a
    | EApp a Ident [Expr a] -> call 
    | EString a String
    | Neg a (Expr a)
    | Not a (Expr a)
    | EMul a (Expr a) (MulOp a) (Expr a)
    | EAdd a (Expr a) (AddOp a) (Expr a)
    | ERel a (Expr a) (RelOp a) (Expr a)
    | EAnd a (Expr a) (Expr a)
    | EOr a (Expr a) (Expr a)
  deriving (Eq, Ord, Show, Read)

SRODOWISKA:
string-literaly  sa slabe, bo jak jest 3 to wloze 3 i ok, jak true, to 1 i ok, ale jak mam stringa to to nie ląduje w kodzie tylko gdzieć i wskaźnik do tego idzie do kodu
musimy je wyciagnac i zastapic tym miejscu osobnym
jak napotykam stringliteral w expr to chce dorzucic do srodowiska strigow (string -> rejestr)

srodowisko na zmienne -> to mozemy stwierdzac, że ona jest w jakims miejscu w pamieci -> prymityw znowu ()
prymityw - zapisz do rejestru x wartosc zmiennej y
- zapisz do rejestru 
- call  f dwojka


3pkt->nie wyliczac dwa razy tej samej rzeczy w obrebie jednego bloku
t3 = 


stmt:
wprowadzamy jeszcze jedn poziom abstrakcji bloki
i na koniec bloku jump do innego bolku

i coś po
lista quatsow, ktora nigdy w srodku nie ma skoku

stmt nie bedzie blokiem
osobnym blokiem -> skok zawsze konczy blok 
w llvm kazdy bolk sie konczy skokiem -> to jest o tyle wygodne, że z tego latwiej sie potem generuje te bloki

bloki tworza graf wierzchlek to blok, a krawedz to jak skacze z jednego do drugiego

kiedys struktura jest nieliniowa if else, while

czasem potrzebuje wygenerowac skoki do labeli, napewno musze miec w petli i w ifie

if (cond) then {stmt} else {stmt} -> to moj stmt
quatsy z warunku, potrzebuje labele do thena i elsa

blok trojka z label1, quatsy, label2/podwojny labelek
lista blokow

cond' <- generatequads cond - osobne dla exp i stmt -> ten kod jest w jakims rejestrze cond' rejestr w ktorym jestwynik (dla rejestrow bedzie to zwracac)
thenLabel <- getLabel - produkuje jakakolwiek nowa etykietke, quats o powstała nowa etykietka, 
elseLabel <- getLabel
endLabel <- getLabel - musza wiedziec dokad skoczysz po wykonaniu
prIntegerjumpa createJump cond' thenLabel elseLabel - czsem skaczemy do thenlebel czasem elselebel, -> musimy zapisac w jakis sposob, ze to jest koniec obecnego bloku conditionaljump
prIntegerpodwojnegolebela tenlabel/elselebel
prIntegerLabel thenLabel - prInteger label
generatequads stmt1
createJump endLabel
prIntegerLabel elseLabel
generateQuads stmt2
createJump endLabel
prIntegerLabel endLabel

while -> skacze sie do warunku, generuje cialo whilea, z konca ciala skaczemy do warunku (z warunku do dwoch) -> etykietki na warunek, cialo, po warunku

monada aktualnie rozpatrywany blok
idziemy statementami -> wyjdzie rozgałę

data Stmt a - generateblock dla tego zioma
    = Empty a  -> nic nie ma quatsa
    | BStmt a (Block a) -> cos bylo w klamerce, czyli my chcemy zrobic nowe srodowisko zmiennych
    | Decl a (Type a) [Item a]
    | Ass a Ident (Expr a)
    | Incr a Ident -> generatequats - doda do akt bloku
    | Decr a Ident
    | Ret a (Expr a) -> zakonczyc blok
    | VRet a -> zakonczyc blok
    | Cond a (Expr a) (Stmt a) -> tez musi miec skok
    | CondElse a (Expr a) (Stmt a) (Stmt a) -> robie podblok
    | While a (Expr a) (Stmt a) -> robie nowy blok
    | SExp a (Expr a) -> generatquads expr
  deriving (Eq, Ord, Show, Read)

generatequats -> 
data Stmt a - generateblock dla tego zioma
    = Empty a  -> nic nie ma quatsa
    | BStmt a (Block a) -> cos bylo w klamerce, czyli my chcemy zrobic nowe srodowisko zmiennych
    | Decl a (Type a) [Item a] -> policzyc te wartosci, jak zalezy od innych zmiennych Integer x = y + 5; -> constexpry
    | Ass a Ident (Expr a) -> wrzuc do rejestru costam ()
    | Incr a Ident -> generatequats - doda do akt bloku ()
    | Decr a Ident arytmetyczna operacja
    | Ret a (Expr a) -> quads return (zawsze konczy blok) wyliczyc expr idzie do starego
    | VRet a -> bra quadsow
    | Cond a (Expr a) (Stmt a) -> tez musi miec skok ->  tylko quadsy warunku
    | CondElse a (Expr a) (Stmt a) (Stmt a) -> robie podblok
    | While a (Expr a) (Stmt a) -> robie nowy blok
    | SExp a (Expr a) -> generatquads expr
  deriving (Eq, Ord, Show, Read)

generatequates, ni ma zadnego generate
bloki sa na potrzeby analizy, co jest kiedy potrzebne -> wywolanie funkcji nic nie zmieni
-}












-- Function starts compilatipn in StateT monad
{-startCompilation :: Program -> String -> IO ()
startCompilation p outfile = do
  evalStateT (compileTree p) (outfile, 1, M.empty)
  return ()-}

{-

-- Function reads file with input program and passes
-- it to parseFunction
getFile :: ParseType (Program ErrorPos) -> FilePath -> IO ()
getFile p f = readFile f >>= parseFile p f


-- Function parses given program, then checks if there are
-- any compilation errors and finally starts compilation
parseFile :: ParseType (Program ErrorPos) -> FilePath -> String -> IO()
parseFile p _ prog_s = 
    let ts = myLexer prog_s in 
        case p ts of
            Bad err -> do 
                hPutStrLn stderr ("ERROR\nParsing failed: " ++ err ++ "\n")
                exitFailure
            Ok tree -> do 
                res <- checkProgram tree
                case res of
                    Nothing -> do
                        hPutStrLn stderr ("OK\n")
                        return ()
                        -- here compiling
                    Just err -> do
                        hPutStrLn stderr ("ERROR\n" ++ err ++ "\n")
                        exitFailure



main :: IO ()
main = do
    args <- getArgs
    case args of
        -- given no arguments
        [] -> do
            putStrLn "Given no arguments to program, usage: latc <filename>"
        (f:[]) -> do
            getFile pProgram f
        -- given too many arguments
        _ -> do
            putStrLn "Invalid number of arguments, usage: insc_jvm <filename>"

-}