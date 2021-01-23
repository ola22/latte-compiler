{-# Options -Wall -Wname-shadowing #-}

module LCSE where


import qualified Data.Map.Strict as M
import Control.Monad.State

import Latte.Parsing.AbsLatte
import IRCreator




type OpResults = M.Map (IRBinOp, IRReg, IRReg) IRReg

type RegResults = M.Map IRReg IRReg

type VarRegisters = M.Map IRReg IRReg

type PtrRegisters = M.Map (IRReg, IRReg) IRReg


type LCSEStore = State (OpResults, RegResults, VarRegisters, PtrRegisters) 





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


checkOpposite :: IRBinOp -> IRReg -> IRReg -> IRReg -> LCSEStore (Maybe IRElem)
checkOpposite op reg1' reg2' reg = do
    (results, regs, vars, ptrs) <- get
    let oposite = (op, reg2', reg1')
    case (M.lookup oposite results) of
        Nothing -> do
            put (M.insert oposite reg results, regs, vars, ptrs)
            return (Just (IROp reg op reg1' reg2'))
        Just res_reg -> do
            put (results, M.insert reg res_reg regs, vars, ptrs)
            return (Nothing)


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
        -- new expression to be evaluated
        Nothing -> do
            case op of
                IRAdd -> checkOpposite op reg1' reg2' reg
                IRTimes -> checkOpposite op reg1' reg2' reg
                _ -> do
                    put (M.insert new_el reg results, regs, vars, ptrs)
                    return (Just (IROp reg op reg1' reg2'))
        -- expression already evaluated
        Just res_reg -> do
            put (results, M.insert reg res_reg regs, vars, ptrs)
            return (Nothing)
optimizeIRElement (IRVarToReg var_reg reg2) = do
    (results, regs, vars, ptrs) <- get
    reg2' <- getCurrentReg reg2
    put (results, regs, M.insert var_reg reg2' vars, ptrs)
    return (Just $ IRVarToReg var_reg reg2')
optimizeIRElement el@(IRVarFromReg reg1 var_reg) = do
    (results, regs, vars, ptrs) <- get
    case (M.lookup var_reg vars) of
        Nothing -> do
            put (results, regs, M.insert var_reg reg1 vars, ptrs)
            return (Just el)
        Just reg -> do
            put (results, M.insert reg1 reg regs, vars, ptrs)
            return (Nothing)
optimizeIRElement (GetPtr reg1 reg2 reg3) = do 
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
optimizeIRElement (PutPtr reg1 reg2 reg3) = do 
    reg1' <- getCurrentReg reg1
    reg2' <- getCurrentReg reg2
    reg3' <- getCurrentReg reg3
    (results, regs, vars, ptrs) <- get
    put (results, regs, vars, M.insert (reg1', reg2') reg3' ptrs)
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
optimizeCond (IRLoc reg) = do
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
    put (M.empty, regs, M.empty, M.empty)
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



