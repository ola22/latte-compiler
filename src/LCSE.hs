{-# Options -Wall -Wname-shadowing #-}

module LCSE where


import qualified Data.Map.Strict as M
import Control.Monad.State

import Latte.Parsing.AbsLatte
import IRCreator

-- a co ze stringami????????? -> chyba ok bo się zastąpią w mapie <3


-- TODO
    -- LABELE KONCOWE TRZA TEZ ZMIENIAC NA KONIEC NO BO SORKA, ALE TRZA IM POMOC, BO NIE WIEDZA GDZIE SKAKC
    -- TE CONDJUMPY I RETY
-- SPRAWDZIC TEST:mamy returna z wyrazeniem, ktore sie powtarza wiele razy, czy da rade i wgl duzo testow
-- BO MOGE TYM SOBIE NIEZLE NAPSUC :<
    -- CO JAK ZMIENNE TEZ BEDE TAKIE SAME PRZEKAZYWAC??? POWINNO SIE COS PSUC!!!!!
-- pointery zeby tez nie ladowac po 10 razy
-- te cale ziomy porownania moze tez wyszukiwac te wyrazenia czy cos
-- te vary i inne kiedy im mapy podmieniac,
-- czy napewno wszytsko aktualizuje, bo co chwila sie cos psuje :/
-- czy wszedzie robie getCurrentReg

-- branie pointerow!!!!!!




-- mapa jakie wyniki juz znam (reg op reg) -> res_reg, 
type OpResults = M.Map (IRBinOp, IRReg, IRReg) IRReg

-- ten rejestr to aktualnie ten rejestr :)
type RegResults = M.Map IRReg IRReg

-- trzymam w jakim rejestrze, ktora zmienna -> czy zmienna jako rejestr???
type VarRegisters = M.Map IRReg IRReg


type PtrRegisters = M.Map (IRReg, IRReg) IRReg  -- TODO 


type LCSEStore = State (OpResults, RegResults, VarRegisters, PtrRegisters) 

{-

trzymam numer quadsa w ktorym jestem, mape z trojek z quadsy(tylko operacji), mapa jakie wyniki juz znam (reg op reg) -> res_reg, 
rejestry -> rejestry   mam zapisane, że a + b jest w x i potem mam y = a + b i potem jak y to ja chce x 
info, kiedy ostatnio zapisana byla dana zmienna -> ze czytam ponownie ze zmiennej, ktorej wartosc znam
wartosci -> rejestry (wykryc, ze to to samo wyrazenie, ze to ta sama zmienna, jak nie bylo pisania do niej)
trzymam w jakim rejestrze, ktora zmienna -> i jak napisze do tej zmiennej to usuwam

mam nowego quadsa:
czytanie ze zmiennej -> jak jest w mapie to wywalam quadsa, i pobieram z mapy I DODAJE DO MAPY TYCH REG -> REG ZE MOJ RESULT TO JEST TEN RESULT Z MAPY
        -> jak go nie ma w mapie to zostawiam quadsa, dodaje do mapy zmienna i jej rejestr chyba nic nie zmieniam <3
pisze do tej zmiennej -> (zapisanie  w pamieci tej liczbowej wartosci)
    -> nie mam jej w mapie (dodaje do mapy) -> chyba musi byc w mapie -> NIEEEE!!!!! nie musi :<
    -> jest w mapie (nadpisujemy to co w mapie) <3
    czy tu sie wgl cokolwiek zmienia??????

operacje -> sprawdzic w mapie kazda zmienna (te rejestry ktorych uzywalam, czy wszystkie zmienne tam sa)
    -> chce stworzyc nowego quadsa -< nadpisuje uzycia rejestrow, ktore nie istnieja, tymi co sa w mapie,
    -> patrze czy ta nowo stworzona gdzies nie istnieje (jak istnieje to wywalam) -> ALE DODAJE DO MAPY ZE MOJ RES_REG TO TAMTEN ZNALEZIONY
    -> a jak nie bylo wyliczone

quad x = a + b
quad y = a + b

op
call
do rej
z rej

jak pomiedzy dwoma odczytami nie mamy zapisu 

-}


{-
data IRElem = 
    IROp IRReg IRBinOp IRReg IRReg | -- arithmetic op (res_reg, op, reg1, reg2)
        IRCall IRReg String [IRReg] | -- func call (res_reg, func_name, args_regs)
        AssignCond IRReg Integer | -- for making boolean expressions
    IRVarToReg IRReg IRReg | -- assigning var (var, location)
    IRVarFromReg IRReg IRReg | -- getting var's val to reg r (r, var)
    GetPtr IRReg IRReg
    deriving Show
-}

{-
AssignVar F.Ident Location                 IRVarToReg IRReg IRReg |
GetVar Location F.Ident                     IRVarFromReg IRReg IRReg
-}


-- JEDEN WIELKI PROBLEM, BO NWM, CZY TO WULICZANIE WARUKU CZY COS, ALE W 
-- JEDNYM BLOKU PRZED CALLEM FUNKCJI REZERWUJE REJESTR DLA KOLEJNEGO BLOKU I KOLEJNY GO UŻYWA, A TAM ZMIENIŁAM
-- ALE TEN KOLEJNY NIE WIE, ZE ja go zmienilam i mamy troche problem :( nawet troche duzy bo nie zdam bez tego :()


-- 1. ja nie wiem, czy w miedzyczasie sie nie zmienily te wartosci w tym op reg reg
-- no bo jak mialam tam var reg no to kiepsko, ale chyba zawsze mam zwykle regi, tak mi sie wydaje
-- 2. to z tymi roznymi zmiennymi i tez w sumie podwyrazeniami, no bo te zmienne to tam podmienilam, a kolejny blok korzystal i tak w sumie niewiadomo co













-------------------------------------- TODO
-- jak z5robie ta mape pointerow, to sprawdzic przypisanie drugie do zmiennej tablicowej, innej tablicy, zeby zobaczyc, czy sie uda takie cuś
-- bo tam bede trzymac te pointery -> i zobaczyc przed i po optymalizacjach
-- test a + b * 2
--      a + b * 3


-- czy sprawdzac przemiennosc podawania i mnożenia, czy TO MOZE NIEZGODNE ZE SPECYFIKACJA ZADANIA?????????????????
-- sprawdzanie reli
-- insert w tych pointerach
-- POTESTOWAC BARDZO DOKLADNIE!!!!!!!!!!!!!!!! -> ALBO LEPIEJ: WYSLAC WCZESNIEJ I zobaczyc co powie :)





getCurrentReg :: IRReg -> LCSEStore IRReg
getCurrentReg reg = do
    (_, regs, _, _) <- get
    case (M.lookup reg regs) of
        Nothing -> return (reg)
        Just reg' -> return (reg')


getCurrentArgsRegs :: [IRReg] -> LCSEStore ([IRReg])
getCurrentArgsRegs [] = return ([])
getCurrentArgsRegs (arg:rest) = do
    arg' <- getCurrentReg arg
    rest' <- getCurrentArgsRegs rest
    return ([arg'] ++ rest')


optimizeIRElement :: IRElem -> LCSEStore (Maybe IRElem)
optimizeIRElement (IRCall res_reg name args) = do
    args' <- getCurrentArgsRegs args
    return (Just (IRCall res_reg name args'))
optimizeIRElement (AssignCond reg x) = do
    reg' <- getCurrentReg reg
    return (Just (AssignCond reg' x))
optimizeIRElement (IROp reg op reg1 reg2) = do
    reg1' <- getCurrentReg reg1
    reg2' <- getCurrentReg reg2
    let new_el = (op, reg1', reg2')
    (results, regs, vars, ptrs) <- get
    case (M.lookup new_el results) of
        Nothing -> do
            -- nie znalezlismy
            -- dodajemy do mapy results tego ziomka new_el -> reg
            -- zwracamy quadsa just (reg new_el)
            put (M.insert new_el reg results, regs, vars, ptrs)
            return (Just (IROp reg op reg1' reg2'))
        Just res_reg -> do
            -- juz to liczylismy
            -- dodajemy do mapy reg->reg, że reg -> res_reg
            -- zwracamy Nothing
            put (results, M.insert reg res_reg regs, vars, ptrs)
            return (Nothing)
optimizeIRElement (IRVarToReg var_reg reg2) = do            -- to jednak nie tak TODO CZY da sie tak, zeby tego nie usuwac??
    (results, regs, vars, ptrs) <- get
    reg2' <- getCurrentReg reg2
    --put (results, regs, M.delete var_reg vars)
    put (results, regs, M.insert var_reg reg2' vars, ptrs)-- czy napewno ok??????????? bo tak jakby podmieniam, że ktoś nie będzie czytał zmiennej ze stosu tylko ot tak z jakiegos rejestru, to po co wgl na stosie ja trzymac pfff
    return (Just $ IRVarToReg var_reg reg2')-- musze zapisac dla innych ziomic z innych blokow, bo oni nie wiedza jakie my tu harce i podmiany organizujemy 
    --return (Nothing)
optimizeIRElement el@(IRVarFromReg reg1 var_reg) = do
    (results, regs, vars, ptrs) <- get
    case (M.lookup var_reg vars) of
        Nothing -> do --- raczej reg nie zostal niczym zastapiony bo go sobie biere pierwszy raz <3
            put (results, regs, M.insert var_reg reg1 vars, ptrs)
            return (Just el)
        Just reg -> do
            put (results, M.insert reg1 reg regs, vars, ptrs)
            return (Nothing)
optimizeIRElement (GetPtr reg1 reg2 reg3) = do       -- TODO JAKAS MAPA NA POINTERY, ZEBY ICH NIE LADOWAC PO WIELE RAZY!!!!!!!!A -> czy ja dostaje pointer czy wskaznik??
    --reg1' <- getCurrentReg reg1
    reg2' <- getCurrentReg reg2
    reg3' <- getCurrentReg reg3
    (results, regs, vars, ptrs) <- get
    case (M.lookup (reg2', reg3') ptrs) of
        Nothing -> do
            put (results, regs, vars, M.insert (reg2', reg3') reg1 ptrs)
            return (Just $ GetPtr reg1 reg2' reg3')
        Just ptr_reg -> do
            put (results, M.insert reg1 ptr_reg regs, vars, ptrs)
            return (Nothing)
optimizeIRElement (PutPtr reg1 reg2 reg3) = do       -- to chce robic zawsze, tak jak var to reg, i czy dodaje do mapy, czy usuwam???????
    reg1' <- getCurrentReg reg1
    reg2' <- getCurrentReg reg2
    reg3' <- getCurrentReg reg3
    (results, regs, vars, ptrs) <- get
    put (results, regs, vars, M.delete (reg1', reg2') ptrs) -- okioki, ten przyklad miedzy dwoma \n\n jest na to :) sprawdz, co jak zrobisz inserta :)
    return (Just $ PutPtr reg1' reg2' reg3')


optimizeIRElements :: [IRElem] -> LCSEStore [IRElem]
optimizeIRElements [] = return ([])
optimizeIRElements (el:rest) = do
    op_el_res <- optimizeIRElement el
    case op_el_res of
        Nothing -> optimizeIRElements rest
        Just op_el -> do
            rest' <- optimizeIRElements rest
            return ([op_el] ++ rest')


optimizeCond :: IRCond -> LCSEStore IRCond
optimizeCond (IRLoc reg) = do       -- TODO czu nie upraszac tez tych operacji porownania???? szukac czy takie dwa juz nie byly ze suba porownywane
    reg' <- getCurrentReg reg
    return (IRLoc reg')
optimizeCond (IRRel op reg1 reg2) = do
    reg1' <- getCurrentReg reg1
    reg2' <- getCurrentReg reg2
    return (IRRel op reg1' reg2')


optimizeEndLabel :: Label -> LCSEStore Label
optimizeEndLabel (RetL reg) = do
    reg' <- getCurrentReg reg
    return (RetL reg')
optimizeEndLabel (CondL cond l1 l2) = do
    cond' <- optimizeCond cond
    return (CondL cond' l1 l2)
optimizeEndLabel l = return (l)


optimizeBlock :: IRBlock -> LCSEStore IRBlock
optimizeBlock (l1, elems, l2) = do
    (_, regs, _, _) <- get
    put (M.empty, regs, M.empty, M.empty)             -- TODO Czy to dobrze robie???    CHYBA MOGE TEZ NIE WYRZUCAC TYCH PODWYRAZEN WEWNATRZ JEDENJ FUNKCYJNKI <3
    elems' <- optimizeIRElements elems
    l2' <- optimizeEndLabel l2
    return ((l1, elems', l2'))

optimizeBlocks :: [IRBlock] -> LCSEStore [IRBlock]
optimizeBlocks [] = return ([])
optimizeBlocks (block:rest) = do
    op_b <- optimizeBlock block
    rest' <- optimizeBlocks rest
    return ([op_b] ++ rest')


optimizeFunc :: ([Ident], [IRBlock]) -> LCSEStore ([Ident], [IRBlock])
optimizeFunc (idents, blocks) = do
    optimized_blocks <- optimizeBlocks blocks
    return ((idents, optimized_blocks))


optimizeLCSE :: [([Ident],[IRBlock])] -> LCSEStore [([Ident],[IRBlock])]
optimizeLCSE [] = return ([])
optimizeLCSE (blocks:rest) = do
    op_f <- optimizeFunc blocks
    rest' <- optimizeLCSE rest
    return ([op_f] ++ rest')


optimizeIRElems :: [([Ident],[IRBlock])] -> [([Ident],[IRBlock])]
optimizeIRElems blocks = 
    evalState (optimizeLCSE blocks) (M.empty, M.empty, M.empty, M.empty) 
