{-# Options -Wall -Wname-shadowing #-}

module Compiler where


import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State

import Latte.Parsing.AbsLatte
import IRCreator



type RegEnv = M.Map String String
type Env = StateT (FilePath, RegEnv, Int) IO
type RegSet = S.Set String


-- TODO ZMIENI
    -- zeby wynik calla byla na koncu, a nie na poczatku
-- te rejestry, żeby były inne
    -- tamto zapamiętywanie i potem przesuwanie, żeby było inaczej
    -- zeby przenazywane zmienne zaczynaly sie od kropki czy podlogi np
    -- zeby funkcje sie nazywaly jakos ladniej w sensie -> main bez podlogi, reszta z podloga koniecznie!!!!!!!!!!
    -- dodac tym strconcat podloge
    -- zeby w rax 

-- zeby inaczej wyliczac warunki
    -- BUG trzeba dwa razy wpisywac liczbe dla readinta
-- pozmieniac troche tego helpera asm
    -- komantarze
-- readme
    -- callling convention
-- jak najbardziej pozmieniac to :/
    -- sprawdzic wszytskie funkcje te ten tego


-- Function returns beginning of assembly file
assemblyBeggining :: String
assemblyBeggining =
    "bits 64\n" ++
    "default rel\n\n" ++
    "global _main\n\n" ++
    "extern _printInt\n" ++
    "extern _printString\n" ++
    "extern _readInt\n" ++
    "extern _readString\n" ++
    "extern _strconcat\n" ++
    "extern _strcmp\n" ++
    "extern _error\n" ++
    "extern _strcmpn\n\n\n"


-- Function returns string containing declarations of 
-- string from compiled latte program in section data
getStringsDecls :: [(String, IRReg)] -> String
getStringsDecls [] = ""
getStringsDecls ((s, (StrLoc s_reg)):rest) = 
    "string" ++ show s_reg ++ " db " ++ s ++ " , 0\n"
        ++ getStringsDecls rest
getStringsDecls _ = undefined


-- Function returns section data of assembly file
getSectionData :: StringStore -> String 
getSectionData s_env = 
    let strings_decls = getStringsDecls (M.toList s_env)
    in "section .data\n\n" ++ strings_decls ++ "\n\n"



--------------------------------------------------------- FUNCTIONS FOR GETTING ALL ir REGISTERS --------------------------------------------------

-- Following functions sreates set containing all 
-- IR registers used in IR representation for given function

-- Function adds IR register to set
addRegToSet :: IRReg -> Env (RegSet)
addRegToSet (Const _) = return (S.empty)
addRegToSet (Reg n) = 
    return (S.insert ("_reg" ++ show n) S.empty)
addRegToSet (StrLoc _) = return (S.empty)
addRegToSet (VarReg name) = 
    return (S.insert name S.empty)


-- Function adds func call registers
getCallRegisters :: [IRReg] -> Env (RegSet)
getCallRegisters [] = return (S.empty)
getCallRegisters (arg:rest) = do
    rest_regs <- getCallRegisters rest
    regs <- addRegToSet arg
    return (S.union rest_regs regs)


-- Fucntion adds IR elem registers
getIRElemRegisters :: IRElem -> Env (RegSet)
getIRElemRegisters (IROp res_reg _ reg1 reg2) = do
    s1 <- addRegToSet res_reg
    s2 <- addRegToSet reg1
    s3 <- addRegToSet reg2
    return (S.union s1 (S.union s2 s3))
getIRElemRegisters (IRCall res_reg _ args_r) = do
    s1 <- addRegToSet res_reg
    args_regs <- getCallRegisters args_r
    return (S.union s1 args_regs)
getIRElemRegisters (IRVarToReg reg1 reg2) = do
    s1 <- addRegToSet reg1
    s2 <- addRegToSet reg2
    return (S.union s1 s2)
getIRElemRegisters (IRVarFromReg reg1 reg2) = do
    s1 <- addRegToSet reg1
    s2 <- addRegToSet reg2
    return (S.union s1 s2)


-- Function adds IR elems registers
getIRElemsRegisters :: [IRElem] -> Env (RegSet)
getIRElemsRegisters [] = return (S.empty)
getIRElemsRegisters (el:rest) = do
    rest_regs <- getIRElemsRegisters rest
    regs <- getIRElemRegisters el
    return (S.union rest_regs regs)


-- Function adds IR registers of given label
getLabelRegisters :: Label -> Env (RegSet)
getLabelRegisters (L _) = return (S.empty)
getLabelRegisters (NoJump) = return (S.empty)
getLabelRegisters (CondL reg _ _) = 
    addRegToSet reg
getLabelRegisters (RetL reg) = 
    addRegToSet reg
getLabelRegisters (VRetL) = return (S.empty)


-- Function adds IR registers of given block
getBlockRegisters :: IRBlock -> Env (RegSet)
getBlockRegisters (_, elems, lebel) = do
    l_regs <- getLabelRegisters lebel
    elems_regs <- getIRElemsRegisters elems
    return (S.union l_regs elems_regs)


-- Function adds IR registers of given blocks
getFuncRegisters :: [IRBlock] -> Env (RegSet)
getFuncRegisters [] = return (S.empty)
getFuncRegisters (block:rest) = do
    rest_regs <- getFuncRegisters rest
    regs <- getBlockRegisters block 
    return (S.union rest_regs regs)


-- Function adds IR registers of function arguments
getArgsRegisters :: [Ident] -> Env (RegSet)
getArgsRegisters [] = return (S.empty)
getArgsRegisters ((Ident name):rest) = do
    let s1 = S.insert name S.empty
    s2 <- getArgsRegisters rest
    return (S.union s1 s2)


----------------------------------------------------- FUNCTIONS FOR ALLOCATING IR REGISTERS STACK -----------------------------------------------------


-- Function maps IR argument's registers to offsets from 
-- stack rbp. Arguments are stored in rsp + 24, rsp + 32, ..
allocateStackToArguments :: Int -> [String] -> Env (Int)
allocateStackToArguments n [] = return (n)
allocateStackToArguments n (reg:rest) = do
    (f, r_m, off) <- get
    put (f, M.insert reg (" + " ++ show n) r_m, off)
    allocateStackToArguments (n + 8) rest


-- Function maps IR registers to offsets from stack rbp
allocateStackToRegisters :: Int -> [String] -> Env (Int)
allocateStackToRegisters n [] = return (n)
allocateStackToRegisters n (reg:rest) = do
    (f, r_m, off) <- get
    put (f, M.insert reg (" - " ++ show n) r_m, off)
    allocateStackToRegisters (n + 8) rest

------------------------------------------------------- FUNCTIONS FOR GENERATING ASSEMBLER CODE -------------------------------------------------------

 
-- Function stores value from asm_reg on stack, 
-- under reg's ir_reg addres
saveOnRegStackLoc :: String -> String -> Env ()
saveOnRegStackLoc ir_reg asm_reg = do
    (f, regs, _) <- get
    case M.lookup ir_reg regs of
        Just offset -> do
            liftIO $ appendFile f $
                "   lea     r14, [rbp" ++ offset ++ "]\n" ++
                "   mov     [r14], " ++ asm_reg ++ "\n"
        Nothing -> undefined
    return ()


-- Function stores value from asm_reg on stack
saveResOnStack :: IRReg -> String -> Env ()
saveResOnStack (Const _) _ = return ()
saveResOnStack (Reg n) asm_reg = 
    saveOnRegStackLoc ("_reg" ++ show n) asm_reg
saveResOnStack (StrLoc _) _ = return ()
saveResOnStack (VarReg name) asm_reg = 
    saveOnRegStackLoc name asm_reg
    

-- Function moves value from stack, from reg's
-- ir_reg addres to asm_reg
saveValFromStack :: String -> String -> Env ()
saveValFromStack ir_reg asm_reg = do
    (f, regs, _) <- get
    case M.lookup ir_reg regs of
        Just offset -> do
            liftIO $ appendFile f $
                "   lea     r14, [rbp" ++ offset ++ "]\n" ++
                "   mov     " ++ asm_reg ++ ", [r14]\n"
        Nothing -> undefined
    return ()


-- Function moves value from stack to asm_reg
saveValInReg :: IRReg -> String -> Env ()
saveValInReg (Const n) asm_reg = do
    (f, _, _) <- get
    liftIO $ appendFile f $
        "   mov     " ++ asm_reg 
        ++ ", " ++ show n ++ "\n"
    return ()
saveValInReg (Reg n) asm_reg = 
    saveValFromStack ("_reg" ++ show n) asm_reg
saveValInReg (StrLoc n) asm_reg = do
    (f, _, _) <- get
    liftIO $ appendFile f $
        "   mov     " ++ asm_reg 
        ++ ", " ++ "string" ++ show n ++ "\n"
    return ()
saveValInReg (VarReg name) asm_reg = 
    saveValFromStack name asm_reg


-- Function put arguments from the list on the stack.
-- Then it will call a function.
putArgsOnStack :: Int -> [IRReg] -> Env ()
putArgsOnStack _ [] = return ()
putArgsOnStack offset (reg:rest) = do
    (f, _, _) <- get
    saveValInReg reg "r13"
    liftIO $ appendFile f $
        "   lea     r11, [rsp + " ++ show offset ++ "]\n" ++
        "   mov     [r11], r13\n"
    putArgsOnStack (offset + 8) rest


-- Function puts expressions values for arithmetic operations 
-- in given registers
putValsInRightRegs :: IRReg -> IRReg -> String -> String -> Env ()
putValsInRightRegs r1 r2 a_r1 a_r2 = do
    saveValInReg r1 a_r1
    saveValInReg r2 a_r2


-- Function geneartes assembly code for given IR element
generateIRElemAssembly :: IRElem -> Env ()
generateIRElemAssembly (IROp res_reg op reg1 reg2) = do
    (f, _ , _) <- get
    case op of
        IRDiv -> putValsInRightRegs reg1 reg2 "rax" "rsi"
        IRMod -> putValsInRightRegs reg1 reg2 "rax" "rsi"
        _ -> putValsInRightRegs reg1 reg2 "r8" "r9"
    case op of
        IRAdd -> do
            liftIO $ appendFile f $
                "   add    r8, r9\n"
            saveResOnStack res_reg "r8"
        IRSub -> do
            liftIO $ appendFile f $
                "   sub     r8, r9\n"
            saveResOnStack res_reg "r8"
        IRTimes -> do
            liftIO $ appendFile f $
                "   imul    r8, r9\n"
            saveResOnStack res_reg "r8"
        IRDiv -> do
            liftIO $ appendFile f $
                "   cqo\n" ++
                "   idiv    rsi\n"
            saveResOnStack res_reg "rax"
        IRMod -> do
            liftIO $ appendFile f $
                "   cqo\n" ++
                "   idiv    rsi\n"
            saveResOnStack res_reg "rdx"
        IRLTH -> do
            liftIO $ appendFile f $
                "   xor     rcx, rcx\n" ++ 
                "   cmp     r8, r9\n" ++
                "   setl    cl\n"
            saveResOnStack res_reg "rcx"
        IRLE -> do
            liftIO $ appendFile f $
                "   xor     rcx, rcx\n" ++ 
                "   cmp     r8, r9\n" ++
                "   setle   cl\n"
            saveResOnStack res_reg "rcx"
        IRGTH -> do
            liftIO $ appendFile f $
                "   xor     rcx, rcx\n" ++ 
                "   cmp     r8, r9\n" ++
                "   setg    cl\n"
            saveResOnStack res_reg "rcx"
        IRGE -> do
            liftIO $ appendFile f $
                "   xor     rcx, rcx\n" ++ 
                "   cmp     r8, r9\n" ++
                "   setge   cl\n"
            saveResOnStack res_reg "rcx"
        IREQU -> do
            liftIO $ appendFile f $
                "   xor     rcx, rcx\n" ++ 
                "   cmp     r8, r9\n" ++
                "   sete    cl\n"
            saveResOnStack res_reg "rcx"
        IRNE -> do
            liftIO $ appendFile f $
                "   xor     rcx, rcx\n" ++ 
                "   cmp     r8, r9\n" ++
                "   setne   cl\n"
            saveResOnStack res_reg "rcx"
    return ()
generateIRElemAssembly (IRCall res_reg f_name args_r) = do
    (f, _, _) <- get
    let offset = (length args_r + 1) * 8
    liftIO $ appendFile f $
        "   sub     rsp, " ++ show offset ++ "\n"

    putArgsOnStack 0 args_r

    liftIO $ appendFile f $
        "   call    " ++ f_name ++ "\n" ++
        "   mov     rax, [rsp + " 
            ++ show (offset - 8) ++ "]\n" ++
        "   add     rsp, " ++ show offset ++ "\n"
    saveResOnStack res_reg "rax"  
    return ()
generateIRElemAssembly (IRVarToReg reg1 reg2) = do
    saveValInReg reg2 "r12"
    saveResOnStack reg1 "r12"
    return ()
generateIRElemAssembly (IRVarFromReg reg1 reg2) = do
    saveValInReg reg2 "r12"
    saveResOnStack reg1 "r12"
    return ()


-- Function generates assembly code for given
-- IR elements.
generateIRElemsAssembly :: [IRElem] -> Env ()
generateIRElemsAssembly [] = return ()
generateIRElemsAssembly (el:rest) = do
    generateIRElemAssembly el
    generateIRElemsAssembly rest


-- Function generates assembly code for given
-- IR block label.
generateLabelAssembly :: Label -> Env ()
generateLabelAssembly (L lebel) = do
    (f, _, _) <- get
    liftIO $ appendFile f $ "   jmp     " ++ lebel
generateLabelAssembly (CondL reg l1 l2) = do
    (f, _, _) <- get
    saveValInReg reg "rax"
    liftIO $ appendFile f $
        "   cmp     rax, 0\n" ++
        "   jne     " ++ l1 ++ "\n" ++
        "   jmp     " ++ l2 ++ "\n"
generateLabelAssembly (RetL reg) = do
    (f, _, off) <- get
    -- we save it in rax due to c standards
    saveValInReg reg "rax" 
    liftIO $ appendFile f $
        "   lea     rdx, [rbp + " 
            ++ show off ++ "]\n" ++
        "   mov     [rdx], rax\n" ++
        "   mov     rsp, rbp\n" ++
        "   pop     rbp\n" ++
        "   ret\n"
generateLabelAssembly (VRetL) = do
    (f, _, _) <- get
    liftIO $ appendFile f $ 
        "   mov     rsp, rbp\n" ++
        "   pop     rbp\n" ++
        "   ret\n"
generateLabelAssembly (NoJump) = return ()


-- Function generates assembly code for given
-- IR block. We have to generate even empty blocks
-- since there are different blocks that use their labels.
generateBlockAssembly :: IRBlock -> Int -> Bool -> Env ()
generateBlockAssembly ((L start_l), elems, end_l) offset is_first = do
    (f, _, _) <- get
    liftIO $ appendFile f $ start_l ++ ":\n"
    if (is_first) then do 
        liftIO $ appendFile f $
            "   push    rbp\n" ++
            "   mov     rbp, rsp\n" ++
            "   sub     rsp, " ++ show offset ++ "\n"
        generateIRElemsAssembly elems
        generateLabelAssembly end_l
    else do 
        generateIRElemsAssembly elems
        generateLabelAssembly end_l
generateBlockAssembly _ _ _ = undefined


-- Function generates assembly code for given
-- IR blocks.
generateBlocksAssembly :: [IRBlock] -> Int -> Bool -> Env ()
generateBlocksAssembly [] _ _ = return ()
generateBlocksAssembly (b:rest) offset is_first = do
    generateBlockAssembly b offset is_first
    (f, _, _) <- get
    liftIO $ appendFile f $ "\n\n"
    generateBlocksAssembly rest offset False


-- Function returns list with arguments'
-- names.
getArgsList :: [Ident] -> Env ([String])
getArgsList [] = return ([])
getArgsList ((Ident name):rest) = do
    rest_ <- getArgsList rest
    return ([name] ++ rest_)


-- Function generates assembly code for given
-- function. Argument args contains list of arguments'
-- names and blocks are IR blocks for considered function.
generateFuncAssembly :: ([Ident],[IRBlock]) -> Env ()
generateFuncAssembly (args, blocks) = do
    -- getting all IR registers 
    regs <- getFuncRegisters blocks
    args_regs <- getArgsRegisters args
    let regs' = regs S.\\ args_regs

    -- allocating stack for function's arguments
    args_list <- getArgsList args
    res_off <- allocateStackToArguments (8*2) args_list

    -- allocating stack for other IR registers
    last_offset <- allocateStackToRegisters 8 (S.toList regs')

    (file, m, _) <- get
    put (file, m, res_off)

    -- generating actual assembly code
    generateBlocksAssembly blocks last_offset True
    (f, _, _) <- get
    liftIO $ appendFile f $ "\n\n"
    return ()


-- Function generates assembly code for each of given
-- functions.
generateFuncsAssembly :: [([Ident],[IRBlock])] -> Env ()
generateFuncsAssembly [] = return ()
generateFuncsAssembly (el:rest) = do
    -- clearing environment for each function's blocks
    (f, _, _) <- get
    put (f, M.empty, 0)
    generateFuncAssembly el
    generateFuncsAssembly rest


-- Function generates assembly code for each list of blocks.
-- Each of list represents one function. Given string store
-- helps with generating strings declarations in data section.
compileBlocks :: [([Ident],[IRBlock])] -> StringStore -> Env ()
compileBlocks blocks s_env = do
    -- adding to asm file file beginning and data section 
    -- with strings' declarations
    (outfile, _, _) <- get
    liftIO $ appendFile outfile $ 
        assemblyBeggining ++ (getSectionData s_env)
    generateFuncsAssembly blocks


-- Function starts compilatipn in StateT monad
-- Given args: IR func_blocks, string_env, out_file
startCompilation :: [([Ident],[IRBlock])] -> StringStore -> String -> IO ()
startCompilation blocks s_env outfile = do
  evalStateT (compileBlocks blocks s_env) (outfile, M.empty, 0)
  return ()

