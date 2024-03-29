{-# Options -Wall -Wname-shadowing #-}

module Main where

import System.FilePath.Posix
import System.Process
import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import System.IO

import Latte.Parsing.ParLatte
import Latte.Parsing.AbsLatte
import Latte.Parsing.ErrM
import Common.ErrorPositions
import SemanticChecker
import IRCreator
import Compiler
--import LCSE
import GCSE




-- Function reads file with input program and passes
-- it to parseFunction
getFile :: ParseType (Program ErrorPos) -> FilePath -> IO ()
getFile p f = readFile f >>= parseFile p f


-- Function parses given program, then checks if there are
-- any compilation errors and finally starts compilation
parseFile :: ParseType (Program ErrorPos) -> FilePath -> String -> IO()
parseFile p f prog_s = 
    let ts = myLexer prog_s in 
        case p ts of
            Bad err -> do 
                hPutStrLn stderr ("ERROR\nParsing failed: " ++ err ++ "\n")
                exitFailure
            Ok tree -> do 
                -- static semantic check
                res <- checkProgram tree
                case res of
                    Nothing -> do
                        hPutStrLn stderr ("OK\n")
                        -- generating IR representation
                        (blocks, strStore) <- getIRRepresentation tree
                        let optimized_blocks = optimizeIRElemsGCSE blocks
                        let outfile = dropExtension f ++ ".s"
                        -- generating assembly code
                        startCompilation optimized_blocks strStore outfile
                        callCommand $ "nasm -g -f elf64 " ++ outfile
                        callCommand $ "gcc " ++ dropExtension f ++ ".o" 
                            ++ " lib/runtime.o lib/runtime_helper.o -o " 
                            ++ dropExtension f
                        return ()
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

