{-# Options -Wall -Wname-shadowing #-}

module SemanticChecker where


import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Error


import Latte.Parsing.LexLatte
import Latte.Parsing.AbsLatte
import Latte.Parsing.ErrM
import Common.ErrorPositions


-- todo
-- typ maina
-- te poziomy deklaracji, kiedy jest multiple declaration, a kiedy nie
-- test 1, 19

--- todo
-- dodawanie tych calych ziomkow, printInt, printCos
-- sprawdzanie typu maian
-- getpos, żeby nie pisać wszędzie tych par getPos -> wtedy bede sie mogla tez ozbyc tych patternow, co nwm, co z nimi zrobic
-- zrobic to cale, gowniane trzymanie boola za kazdym razem jak wchodze do blocka robi podmianke 


-- OGOLNE WATPLIWOSCI
-- czy to tak ma byc, ze wszedzie bede je sb wywolywac, nie patrzec na wynik, bo chce zeby najwyzej mi zrobil throwerror
-- zrobic funkcje getpo
-- czy throwError przewywa mi wszytskie ziomki i nie musze łapać?
-- co sprawdzać w argumentach funkcji???
-- czy multiple declaration of functions może byc ??????
-- co z tym multipledeclaration, kiedy jest, a kiedy nie


-- z polecenie
-- stringi jako argumenty przez referencje
-- czy funkcja moze byc argumentem




-- zmienne
-- typy
-- returny w osiąglanych branchach

-- osobne środowisko funkcji i osobne zmiennych
-- przechodzimy po drzewku i 
-- jak mamy zmienna to jej typ znamy -> jak nie byla zadeklerowana to blad
-- jedyna rzecz co ma rozne typy to plus, rowna sie 

-- jak jestesmy w wywolaniu funkcji to typ funkcji i argumenty
-- jak while to warunek i stmt w ciele whilea
-- w ifie czy warunek jest boolem
-- czy obie galezie tego samego typu

-- returny:
-- patrzymy,czy ostatnia istrukcja funkcji jest return albo if czy w obu galeziach jest return
-- unreachable called albo 

-- przejsc sobie po wszystkich funkcjach i pamietac ich typy
-- dodac do srodowiska funkcji printInt printstring 
-- na koniec sprawdxic, czy jest main
-- MAMY SRODOWISKO FUNKCJI

-- drugie przejscie -> po funkcjach i ciala tych funkcji
-- arg funkcji jako zmienne w srodowisku 
-- run monad osobno dla kazdej funkcji




type ParseType a = [Token] -> Err a

type FunStore = (M.Map Ident (Type ErrorPos))
type VarStore = (M.Map Ident (Type ErrorPos))

-- ident to  aktualnie rozpatrywana funckja
type ProgChecker = StateT (FunStore, VarStore, Type ErrorPos) (ErrorT String IO)


-- TODO PRZENIESC GDZIE DO OSOBNEGO PLICZKU TE FUNKCYJNKI
getName :: Ident -> String
getName (Ident name) = name


getIden :: Item ErrorPos -> Ident
getIden (Init _ iden _) = iden
getIden (NoInit _ iden) = iden


typesEquals :: Type ErrorPos -> Type ErrorPos -> Bool
typesEquals (Int _) (Int _) = True
typesEquals (Str _) (Str _) = True
typesEquals (Bool _) (Bool _) = True
typesEquals (Void _) (Void _) = True
typesEquals (Fun _ _ _) (Fun _ _ _) = True -- TODO sprawdzic typy tych argumrntow tez wszytskich
typesEquals _ _ = False


libFuns :: [(Ident, Type ErrorPos)]
libFuns = [((Ident "printInt"), (Fun Nothing (Void Nothing) [Int Nothing])),
            ((Ident "printString"), (Fun Nothing (Void Nothing) [Str Nothing])),
            ((Ident "error"), (Fun Nothing (Void Nothing) [])),
            ((Ident "readInt"), (Fun Nothing (Int Nothing) [])),
            ((Ident "readString"), (Fun Nothing (Str Nothing) []))]



--------------------------------------------------- PREPEARING FUNCS ENVIRONMENT -----------------------------------------------------

-- Function returns a list of arguments' types from given
-- list of arguments.
getArgsTypes :: [Arg ErrorPos] -> [Type ErrorPos] -> [Type ErrorPos]
getArgsTypes [] types = types             -- czy dobra kolejność argumentow przetestowac na wieloargumentowych roznych typow
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
            then (funcs, Just $ "Multiple declaration of function " 
                ++ getName iden ++ " in line " ++ (getLineNumber pos))
        else (
            if ((getName iden) == "main") 
                then (
                    case typ of
                        (Int _) -> (getFuncs rest (M.insert iden func_typ funcs) True)
                        _ -> (funcs, Just $ "Invalid type of main function in line " 
                            ++ getLineNumber pos ++ ": main must be of int type")
                )
            else (getFuncs rest (M.insert iden func_typ funcs) is_main)
    )


-- Function adds library functions to functions' environment.
addLibFuncs :: FunStore -> FunStore
addLibFuncs funs = 
    let lib_funs = libFuns in
        --foldr (+) 0 veryBigList
        M.union funs (M.fromList lib_funs)



--------------------------------------------------- ADDING FUNCTION'S ARGUMENTS TO ENV --------------------------------------------------


-- TODO zmienic to , żeby w tych dwoch pisalo blad, a reszta poporstu return
-- Function checks if given function's argument has valid type.
-- Invalid are Void and Fun.
checkArgType :: Ident -> Type ErrorPos -> ProgChecker ()
checkArgType _ (Int _) = return ()
checkArgType _ (Str _) = return ()
checkArgType _ (Bool _) = return ()
checkArgType iden (Void pos) = 
    throwError ("Invalid argument's type void of argument " 
        ++ (getName iden) ++ " in line " ++ (getLineNumber pos))
checkArgType iden (Fun pos _ _) = 
    throwError ("Invalid argument's type of argument " 
        ++ (getName iden) ++ " in line " ++ (getLineNumber pos))   -- TODO CZY TO NAPEWNO DOBRZE?????


-- TODO czy dobrze to spr typow 
-- Function adds function's arguments to variables' environment.
-- It also checks theirs correctness.
addArgsToEnv :: [Arg ErrorPos] -> ProgChecker ()
addArgsToEnv [] = return ()
addArgsToEnv ((Arg pos typ iden):rest) = do
    (funs, args, f) <- get
    if (M.member iden args) then do 
        throwError ("Multiple declaration of argument " ++ (getName iden) 
            ++ " in line " ++ (getLineNumber pos) ++ " in function's arguments.")
    else do 
        checkArgType iden typ
        put (funs, (M.insert iden typ args), f)
        addArgsToEnv rest


----------------------------------------------- CHECKING FUNCTION'S BODY (STATEMENTS) ------------------------------------------------


-- TODO spr typ wyrazenia exp, czy sie zgadza z tym, ale musze mieć exp napisane, bo bedzie dawac typ
-- data Item a = NoInit a Ident | Init a Ident (Expr a)
checkInit :: Item ErrorPos -> Type ErrorPos -> ProgChecker ()
checkInit (Init pos iden e) decl_typ = do
    e_typ <- checkExp e
    if (typesEquals e_typ decl_typ) 
        then return ()
    else throwError ("In line " ++ getLineNumber pos 
        ++ " initial value's type doesn't match declared type. " ++ show e_typ ++ " " ++ show decl_typ)


-- Int a | Str a | Bool a | Void a | Fun a (Type a) [Type a]
-- czy moga byc funkcje wewnatrz funkcji deklarowane
-- czy moga byc te same nazwy zmiennych
-- a te same funkje, co zmienne???
-- TODO to jest źle, bo wewnątrz blokow powinno sie nadpisac :/
-- TODO przetestowac, czy dziala to check init wgl???? -> czy sie nie psuje
addVarsToEnv :: Type ErrorPos -> [Item ErrorPos] -> ProgChecker ()
addVarsToEnv _ [] = return ()
addVarsToEnv typ (var:rest) = do
    (funs, vars, f) <- get
    let iden = getIden var
    (if (M.member iden vars) then do 
        throwError ("Multiple declaration of variable " ++ (getName iden) 
            ++ " in line " ++ (getPosItem var) ++ " in function's body.")
    else do 
        case typ of
            Int _ -> put (funs, (M.insert iden typ vars), f)
            Str _ -> put (funs, (M.insert iden typ vars), f)
            Bool _ -> put (funs, (M.insert iden typ vars), f)
            Void _ -> throwError ("Invalid variable's " ++ (getName iden) 
                        ++ " type void in line " ++ getPosItem var)
            Fun _ _ _ -> throwError ("Invalid variable's " ++ getName iden 
                            ++ " type in line " ++ getPosItem var))
    case var of  -- chyba ok
        Init pos iden' e -> do
            checkInit (Init pos iden' e) typ
            addVarsToEnv typ rest
        _ -> addVarsToEnv typ rest
    

-- Checking decrement and increment statements.
checkIncrAndDecr :: Ident -> String -> ErrorPos -> ProgChecker ()
checkIncrAndDecr iden operation pos = do
    (funs, vars, _) <- get
    let var_typ = M.lookup iden vars
    case var_typ of
        Nothing -> 
            throwError ("Trying to " ++ operation ++ " undeclared variable " 
                ++ (getName iden) ++ " in line " ++ (getLineNumber pos))
        Just decl_typ -> do
            case decl_typ of
                Int _ -> return ()
                _ -> throwError ("Trying to " ++ operation ++ " variable " ++ (getName iden) ++ 
                    ",which is not int type in line " ++ (getLineNumber pos))


-- Checking if and if-else statements.
checkIf :: ErrorPos -> Expr ErrorPos -> Stmt ErrorPos -> Stmt ErrorPos -> ProgChecker ()
checkIf pos e stmt1 stmt2 = do
    e_typ <- checkExp e
    case e_typ of
        (Bool _) -> do
            checkStmt stmt1       -- TODO TU NOE TRZEBA SPR TYPOW GALEZI, CZY TAKIE SAME, ANI NIC???
            checkStmt stmt2
        _ -> throwError ("In if statement in line " ++ getLineNumber pos 
                ++ " given condition of non-boolean type.")

{-
Empty a  -- zwasze poprawne
    | BStmt a (Block a) -- lista stmt, kazdy ok
    | Decl a (Type a) [Item a] -- decl zmiennej dodaje do środowiska i jezeli z przypisanie z poczatkowa wartoscia to spr typy
    | Ass a Ident (Expr a) -- spr czy istnieje dana zmienna, spr typ wyrażenia i czy typy sie zgadzaja -> jakos zaznaczam, że zainincjalizowana
    | Incr a Ident -- zmienna musi byc typu int
    | Decr a Ident
    | Ret a (Expr a) -- return zostawiamy na koniec, napewno czy sie zgadza z typem funkcji
    | VRet a  -- voname return, czy typy sie zgadza
    | Cond a (Expr a) (Stmt a) -- spr czy warunek jest boolem, czy stamt sie zgadzaja
    | CondElse a (Expr a) (Stmt a) (Stmt a) -- dwa warunki do spr, dwa stmt do spr
    | While a (Expr a) (Stmt a) -- spr warunek i czy cialo while jest ok
    | SExp a (Expr a) -- poporsotu wyrazenie
-}
checkStmt :: Stmt ErrorPos -> ProgChecker ()
checkStmt (Empty _) = return () -- ok
checkStmt (BStmt _ (Block _ stmts)) = do
    (_, vars, _) <- get
    checkFuncBody vars stmts
checkStmt (Decl _ typ vars) = do
    addVarsToEnv typ vars 
    return ()   -- musze kazda tam sobie dodac
checkStmt (Ass pos iden e) = do
    (funs, vars, _) <- get
    let var_typ = M.lookup iden vars
    case var_typ of
        Nothing -> 
            throwError ("Trying to assign undeclared variable " 
                ++ (getName iden) ++ " in line " ++ (getLineNumber pos))
        Just decl_typ -> do
            e_typ <- checkExp e
            if (typesEquals decl_typ e_typ) then
                return ()
            else
                throwError ("Trying to assign value of wrong type to variable " 
                ++ (getName iden) ++ " in line " ++ (getLineNumber pos))
checkStmt (Incr pos iden) = checkIncrAndDecr iden "increase" pos
checkStmt (Decr pos iden) = checkIncrAndDecr iden "decrease" pos
checkStmt (Ret pos e) = do
    e_typ <- checkExp e
    (_, _, fun_typ) <- get
    if (typesEquals e_typ fun_typ) 
        then return ()
    else throwError ("In line " ++ getLineNumber pos 
        ++ " returned value doesn't match function's type.")
checkStmt (VRet pos) = do
    (_, _, fun_typ) <- get
    if (typesEquals (Void Nothing) fun_typ) 
        then return ()
    else throwError ("In line " ++ getLineNumber pos 
        ++ " returned value doesn't match function's type.")
checkStmt (Cond pos e stmt) = checkIf pos e stmt (Empty Nothing)
checkStmt (CondElse pos e stmt1 stmt2) = 
    checkIf pos e stmt1 stmt2
checkStmt (While pos e stmt) = do            -- CZY TU SPRAWDZAMY, CZY NIE JEST JAKIES WHILE TRUE, CZY COS
    e_typ <- checkExp e
    case e_typ of
        (Bool _) -> checkStmt stmt
        _ -> throwError ("In if statement in line " ++ getLineNumber pos 
                ++ " given condition of non-boolean type.")
checkStmt (SExp pos e) = do
    checkExp e
    return ()



------------------------------------------------------ CHECKING EXPRESSIONS -----------------------------------------------

-- Function checks if arguments used in function's call are of the same types, as
-- in function's declaration.
checkArgsTypes :: [Type ErrorPos] -> [Expr ErrorPos] -> Integer -> ProgChecker ()
checkArgsTypes [] [] _ = return () 
checkArgsTypes (arg_typ:types) (e:exprs) num = do
    e_typ <- checkExp e
    if (typesEquals e_typ arg_typ) then 
        checkArgsTypes types exprs (num + 1)
    else
        throwError ("In function's application wrong type of argument number " 
            ++ show num ++ " in line " ++ getPosExpr e)
checkArgsTypes _ _ _ = return ()


-- Function checks if number of arguments used in function's call is the
-- same as in function's declaration.
checkFuncArgs :: Type ErrorPos -> [Expr ErrorPos] -> ProgChecker ()
checkFuncArgs (Fun pos typ arg_types) args = do
    let len1 = length arg_types
    let len2 = length args
    if (len1 /= len2) then 
        throwError ("Wrong number of arguments in function application. Given " 
        ++ show len2 ++ " args instead of " ++ show len1 ++ " in line " ++ getLineNumber pos)
    else do
        checkArgsTypes arg_types args 1
checkFuncArgs _ _ = return () -- tu chyba nic nie jest potrzebne 


-- Fucntion checks if boolean operations are applied to expr of boolean 
-- type.
checkBooleanOperators :: ErrorPos -> Expr ErrorPos -> Expr ErrorPos -> String -> ProgChecker (Type ErrorPos)
checkBooleanOperators pos e1 e2 op = do
    e1_typ <- checkExp e1
    e2_typ <- checkExp e2
    if (typesEquals e1_typ e2_typ) then do
        -- we know that types are the same
        case e1_typ of
            (Bool _) -> return e1_typ            -- czy napewno pozycje e1 chce dawac??
            typ -> throwError ("Trying apply " ++ op 
                ++ " operation to expressions, which are not boolean type " 
                ++ " in line " ++ (getPosType e1_typ))
    else throwError ("Mistached expressions' types in " ++ op ++ " operation in line " 
                        ++ (getPosType e1_typ))




{-
data Expr a
    = EVar a Ident -- spr czy zadklearowana, zainicjalizowana, i jakiego typu  -> zaicjalizowana nie trzeba sprawdzac!!!!!
    | ELitInt a Integer  --int
    | ELitTrue a --bool
    | ELitFalse a -- bool
    | EApp a Ident [Expr a] -- aplikacja funkcji spr, czy zadeklarowana, ze srodowiska funkcji typ, liczba argumentow, typ argumentow
    | EString a String -- string
    | Neg a (Expr a) -- - przed intem, czy int
    | Not a (Expr a) -- na boolach, czy bool
    | EMul a (Expr a) (MulOp a) (Expr a) -- typy obydwu exprow i musza byc to inty wiec te beda jakos zwracac typy
    | EAdd a (Expr a) (AddOp a) (Expr a)
    | ERel a (Expr a) (RelOp a) (Expr a) -- relacja <,>,<= RelOp
    | EAnd a (Expr a) (Expr a)
    | EOr a (Expr a) (Expr a)
  deriving (Eq, Ord, Show, Read)-}
-- TODO zrobic swoj własny typ tych ziomkow, zeby nie sprawdac pozycji
-- TODO  poprzerabiac te wsyztskie funckje zeby nie dublowac kodu
checkExp :: Expr ErrorPos -> ProgChecker (Type ErrorPos) 
checkExp (EVar pos iden) = do  -- todo czy to wszytsko??
    (_, vars, _) <- get
    let var_typ = M.lookup iden vars
    case var_typ of
        Nothing -> 
            throwError ("Trying to use undeclared variable " 
                ++ (getName iden) ++ " in line " ++ (getLineNumber pos))
        Just decl_typ -> return decl_typ
checkExp (ELitInt pos _) = return (Int pos) 
checkExp (ELitTrue pos) = return (Bool pos) 
checkExp (ELitFalse pos) = return (Bool pos) 
checkExp (EApp pos iden exprs) = do
    (funs, _, _) <- get
    let fun_typ = M.lookup iden funs
    case fun_typ of
        Nothing -> 
            throwError ("Trying to use undeclared function " 
                ++ (getName iden) ++ " in line " ++ (getLineNumber pos))
        Just (Fun pos ret_typ arg_types) -> do
            checkFuncArgs (Fun pos ret_typ arg_types) exprs
            return ret_typ
checkExp (EString pos _) = return (Str pos)    -- wiem napewno, że to string cn???
checkExp (Neg pos e) = do
    e_typ <- checkExp e
    case e_typ of
        (Int _) -> return e_typ
        typ -> throwError ("Trying to negate expression, which is not integer " 
                ++ " in line " ++ (getPosType typ))
checkExp (Not pos e) = do
    e_typ <- checkExp e
    case e_typ of
        (Bool _) -> return e_typ
        typ -> throwError ("Trying to apply negation expression, which is not integer " 
                ++ " in line " ++ (getPosType typ))
checkExp (EMul pos e1 op e2) = do
    e1_typ <- checkExp e1
    e2_typ <- checkExp e2
    case e1_typ of
        (Int _) -> do
            case e2_typ of
                (Int _) -> return e1_typ            -- czy napewno pozycje e1 chce dawac??
                typ -> throwError ("Trying apply multiplication to right expression, which is not integer " 
                        ++ " in line " ++ (getPosType typ))
        typ -> throwError ("Trying apply multiplication to left expression, which is not integer " 
                        ++ " in line " ++ (getPosType typ))
checkExp (EAdd pos e1 op e2) = do     -- TODO czy jak w add jest minus to tez dla stingow dziala???
    e1_typ <- checkExp e1
    e2_typ <- checkExp e2
    if (typesEquals e1_typ e2_typ) then do
        -- we know that types are the same
        case e1_typ of
            (Int _) -> return e1_typ            -- czy napewno pozycje e1 chce dawac??
            (Str _) -> return e1_typ
            typ -> throwError ("Trying apply addition operation to expressions, which are not integer type " 
                            ++ " in line " ++ (getPosType e1_typ))
    else throwError ("Mistached expressions' types in addition operation in line " 
                        ++ (getPosType e1_typ))
checkExp (ERel pos e1 op e2) = do
    e1_typ <- checkExp e1
    e2_typ <- checkExp e2
    if (typesEquals e1_typ e2_typ) then do
        case op of
            EQU pos -> do
                case e1_typ of
                    (Bool _) -> return (Bool pos)            -- czy napewno pozycje e1 chce dawac??
                    (Str _) -> return (Bool pos)
                    (Int _) -> return (Bool pos)
                    typ -> throwError ("Trying apply = operation to expressions, which are not boolean, int or string type " 
                                    ++ " in line " ++ (getLineNumber pos))
            NE pos -> do
                case e1_typ of
                    (Bool _) -> return (Bool pos)            -- czy napewno pozycje e1 chce dawac??
                    (Str _) -> return (Bool pos)
                    (Int _) -> return (Bool pos)
                    typ -> throwError ("Trying apply != operation to expressions, which are not boolean, int or string type " 
                                    ++ " in line " ++ (getLineNumber pos))
            _ -> do
                case e1_typ of
                        (Int _) -> return (Bool pos)
                        typ -> throwError ("Trying apply rel operation to expressions, which are not int type " 
                                        ++ " in line " ++ (getLineNumber pos)) 
    else throwError ("Mistached expressions' types in rel operation in line " 
                                ++ (getPosType e1_typ))
checkExp (EAnd pos e1 e2) = checkBooleanOperators pos e1 e2 "and"
checkExp (EOr pos e1 e2) = checkBooleanOperators pos e1 e2 "or"


-- Function checks correctness od whole function's body.
-- After checking given block of statements it restores old 
-- variables' environment, since variables declared in block are
-- not visible outside given block.
checkFuncBody :: VarStore -> [Stmt ErrorPos] -> ProgChecker ()      -- TUUUUUU
checkFuncBody vars [] = do 
    (funs, _, typ) <- get     -- TUUUUUU
    put (funs, vars, typ)     -- TUUUUUU
    return ()
checkFuncBody vars (stmt:rest) = do
    checkStmt stmt
    checkFuncBody vars rest


--------------------------------------------------- CHECKING PROGRAM'S CORRECTNESS --------------------------------------------------=

-- TODO czy jakos sprawdzamy typ samej funkcji??? -> pewnie dopiero z returnami
-- Function checks given top level definition of type FnDef
checkFunction :: TopDef ErrorPos -> ProgChecker ()
checkFunction (FnDef _ typ _ args (Block _ stmts)) = do
    addArgsToEnv args
    (funs, vars, _) <- get      -- TUUUUUU
    put (funs, vars, typ)              -- TUUUUUUUUUUUU
    checkFuncBody vars stmts  -- chyba sam blok starczy, bo reszte mama w środowisku


-- TODO poki co nie potrzebuje wcale tej funckji
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
    let funcs' = addLibFuncs funcs      -- czy to dobrze?????
    case err of
        Nothing -> do
            res <- runErrorT (runStateT (checkProgramElems prog) 
                            (funcs', M.empty, Void Nothing))
            case res of
                Left err1 -> return (Just err1)
                Right _ -> return Nothing                                             -- CZY TO NAPEWNO DOBRZE ROZUMEIM??????????????????
        Just err2 -> return (Just err2)


