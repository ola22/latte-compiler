{-# Options -Wall -Wname-shadowing #-}

module GCSE where


import qualified Data.Map.Strict as M
import Control.Monad.State

import Latte.Parsing.AbsLatte
import IRCreator





type OpResults = M.Map (IRBinOp, IRReg, IRReg) IRReg

type RegResults = M.Map IRReg IRReg

type VarRegisters = M.Map IRReg IRReg

type PtrRegisters = M.Map (IRReg, IRReg) IRReg

type StoresMap = M.Map Label (OpResults, RegResults, VarRegisters, PtrRegisters)


type GCSEStore = State (Label, StoresMap) 






getLabelStore :: GCSEStore (OpResults, RegResults, VarRegisters, PtrRegisters)
getLabelStore = do
    (l, stores) <- get
    case (M.lookup l stores) of
        Nothing -> return (M.empty, M.empty, M.empty, M.empty)
        Just store -> return (store)


getCurrentReg :: IRReg -> GCSEStore IRReg
getCurrentReg reg = do
    (_, regs, _, _) <- getLabelStore
    case (M.lookup reg regs) of
        Nothing -> return (reg)
        Just reg' -> return (reg')


getCurrentArgsRegs :: [IRReg] -> GCSEStore ([IRReg])
getCurrentArgsRegs [] = return ([])
getCurrentArgsRegs (arg:rest) = do
    arg' <- getCurrentReg arg
    rest' <- getCurrentArgsRegs rest
    return ([arg'] ++ rest')


checkOpposite :: IRBinOp -> IRReg -> IRReg -> IRReg -> GCSEStore (Maybe IRElem)
checkOpposite op reg1' reg2' reg = do
    (results, regs, vars, ptrs) <- getLabelStore
    let oposite = (op, reg2', reg1')
    case (M.lookup oposite results) of
        Nothing -> do
            (l, stores) <- get
            put (l, M.insert l (M.insert oposite reg results, 
                regs, vars, ptrs) stores)
            return (Just (IROp reg op reg1' reg2'))
        Just res_reg -> do
            (l, stores) <- get
            put (l, M.insert l (results, 
                M.insert reg res_reg regs, vars, ptrs) stores)
            return (Nothing)


optimizeIRElement :: IRElem -> GCSEStore (Maybe IRElem)
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
    (results, regs, vars, ptrs) <- getLabelStore
    case (M.lookup new_el results) of
        -- new expression to be evaluated
        Nothing -> do
            case op of
                IRAdd -> checkOpposite op reg1' reg2' reg
                IRTimes -> checkOpposite op reg1' reg2' reg
                _ -> do
                    (l, stores) <- get
                    put (l, M.insert l (M.insert new_el reg results, 
                        regs, vars, ptrs) stores)
                    return (Just (IROp reg op reg1' reg2'))
        -- expression already evaluated
        Just res_reg -> do
            (l, stores) <- get
            put (l, M.insert l (results, 
                M.insert reg res_reg regs, vars, ptrs) stores)
            return (Nothing)
optimizeIRElement (IRVarToReg var_reg reg2) = do
    (results, regs, vars, ptrs) <- getLabelStore
    reg2' <- getCurrentReg reg2
    (l, stores) <- get
    put (l, M.insert l (results, regs, 
        M.insert var_reg reg2' vars, ptrs) stores)
    return (Just $ IRVarToReg var_reg reg2')
optimizeIRElement el@(IRVarFromReg reg1 var_reg) = do
    (results, regs, vars, ptrs) <- getLabelStore
    case (M.lookup var_reg vars) of
        Nothing -> do
            (l, stores) <- get
            put (l, M.insert l (results, regs, 
                M.insert var_reg reg1 vars, ptrs) stores)
            return (Just el)
        Just reg -> do
            (l, stores) <- get
            put (l, M.insert l (results, M.insert reg1 reg regs, 
                vars, ptrs) stores)
            return (Nothing)
optimizeIRElement (GetPtr reg1 reg2 reg3) = do 
    reg2' <- getCurrentReg reg2
    reg3' <- getCurrentReg reg3
    (results, regs, vars, ptrs) <- getLabelStore
    case (M.lookup (reg2', reg3') ptrs) of
        Nothing -> do
            (l, stores) <- get
            put (l, M.insert l (results, regs, vars, 
                M.insert (reg2', reg3') reg1 ptrs) stores)
            return (Just $ GetPtr reg1 reg2' reg3')
        Just ptr_reg -> do
            (l, stores) <- get
            put (l, M.insert l (results, 
                M.insert reg1 ptr_reg regs, vars, ptrs) stores)
            return (Nothing)
optimizeIRElement (PutPtr reg1 reg2 reg3) = do 
    reg1' <- getCurrentReg reg1
    reg2' <- getCurrentReg reg2
    reg3' <- getCurrentReg reg3
    (results, regs, vars, ptrs) <- getLabelStore
    (l, stores) <- get
    put (l, M.insert l (results, regs, vars, 
        M.insert (reg1', reg2') reg3' ptrs) stores)
    return (Just $ PutPtr reg1' reg2' reg3')


optimizeIRElements :: [IRElem] -> GCSEStore [IRElem]
optimizeIRElements [] = return ([])
optimizeIRElements (el:rest) = do
    op_el_res <- optimizeIRElement el
    case op_el_res of
        Nothing -> optimizeIRElements rest
        Just op_el -> do
            rest' <- optimizeIRElements rest
            return ([op_el] ++ rest')


optimizeCond :: IRCond -> GCSEStore IRCond
optimizeCond (IRLoc reg) = do
    reg' <- getCurrentReg reg
    return (IRLoc reg')
optimizeCond (IRRel op reg1 reg2) = do
    reg1' <- getCurrentReg reg1
    reg2' <- getCurrentReg reg2
    return (IRRel op reg1' reg2')


optimizeEndLabel :: Label -> GCSEStore Label
optimizeEndLabel (RetL reg) = do
    reg' <- getCurrentReg reg
    return (RetL reg')
optimizeEndLabel (CondL cond l1 l2) = do
    cond' <- optimizeCond cond
    return (CondL cond' l1 l2)
optimizeEndLabel l = return (l)


mergeVarsMaps :: VarRegisters -> VarRegisters -> VarRegisters
mergeVarsMaps m1 m2 = 
    let f key data1 result = 
            case (M.lookup key m2) of
                Just data2 -> 
                    if (data1 == data2) then
                        M.insert key data1 result
                    else result
                Nothing -> result
    in M.foldrWithKey f M.empty m1


mergePtrsMaps :: PtrRegisters -> PtrRegisters -> PtrRegisters
mergePtrsMaps m1 m2 = 
    let f key data1 result = 
            case (M.lookup key m2) of
                Just data2 -> 
                    if (data1 == data2) then
                        M.insert key data1 result
                    else result
                Nothing -> result
    in M.foldrWithKey f M.empty m1


putStore :: Label -> GCSEStore ()
putStore l2 = do
    (l1, stores) <- get
    store@(res, rgs, vrs, pts) <- getLabelStore
    case (M.lookup l2 stores) of
        Nothing -> do
            put (l1, M.insert l2 store stores)
            return ()
        Just (_, _, vars, ptrs) -> do
            put (l1, M.insert l2 (res, rgs, mergeVarsMaps vrs vars, 
                mergePtrsMaps pts ptrs) stores)
            return ()


putStoreInMap :: Label -> Label -> GCSEStore ()
putStoreInMap l2@(L _) _ = do
    putStore l2
putStoreInMap (CondL _ l3 l4) _ = do
    putStore (L l3)
    putStore (L l4)
putStoreInMap (NoJump) l3 = do
    putStore l3
putStoreInMap _ _ = return ()


optimizeBlock :: IRBlock -> Label -> GCSEStore IRBlock
optimizeBlock (l1, elems, l2) l3 = do
    (_, stores) <- get
    put (l1, stores)
    elems' <- optimizeIRElements elems
    l2' <- optimizeEndLabel l2
    putStoreInMap l2 l3
    return ((l1, elems', l2'))


getFirstLabel :: [IRBlock] -> Label
getFirstLabel ((l1, _, _):_) = l1
getFirstLabel [] = L ""


optimizeBlocks :: [IRBlock] -> GCSEStore [IRBlock]
optimizeBlocks [] = return ([])
optimizeBlocks (block:rest) = do
    let l1 = getFirstLabel rest
    op_b <- optimizeBlock block l1
    rest' <- optimizeBlocks rest
    return ([op_b] ++ rest')


optimizeFunc :: ([Ident], [IRBlock]) -> GCSEStore ([Ident], [IRBlock])
optimizeFunc (idents, blocks) = do
    (results, regs, _, _) <- getLabelStore
    let l1 = getFirstLabel blocks
    put (l1 , M.insert l1 
        (results, regs, M.empty, M.empty) M.empty)
    optimized_blocks <- optimizeBlocks blocks
    return ((idents, optimized_blocks))


optimizeGCSE :: [([Ident],[IRBlock])] -> GCSEStore [([Ident],[IRBlock])]
optimizeGCSE [] = return ([])
optimizeGCSE (blocks:rest) = do
    op_f <- optimizeFunc blocks
    rest' <- optimizeGCSE rest
    return ([op_f] ++ rest')


optimizeIRElemsGCSE :: [([Ident],[IRBlock])] -> [([Ident],[IRBlock])]
optimizeIRElemsGCSE blocks = 
    evalState (optimizeGCSE blocks) (L "", M.empty) 

