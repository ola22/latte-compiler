{-# Options -Wall -Wname-shadowing #-}

module Main where

--import System.FilePath.Posix
--import System.Process
import System.Environment ( getArgs )
import System.Exit ( exitFailure )
--import qualified Data.Map.Strict as M
--import qualified Data.Set as S
--import Control.Monad.State

import System.IO

--import Latte.Parsing.LexLatte
import Latte.Parsing.ParLatte
import Latte.Parsing.AbsLatte
import Latte.Parsing.ErrM
import Common.ErrorPositions
import SemanticChecker


-- TODO
-- dobre kod 0, ale na stderr "OK"
-- zle kod != 0, i na stdeerrr ERROR
-- nie ma takiego pliku???


-- type ParseType a = [Token] -> Err a




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
                hPutStrLn stderr ("ERROR\nParsing failed: " ++ err)
                exitFailure                                   -- CZY KOD BEDZIE ROZNY OD 0
            Ok tree -> do 
                res <- checkProgram tree                       -- jakos wylapywac te bledziki monady error czy cos
                case res of
                    Nothing -> do
                        hPutStrLn stderr ("OK\n")
                        return ()
                        -- tutaj kompiluje
                    Just err -> do
                        hPutStrLn stderr ("ERROR\n" ++ err)
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
        --callCommand ("llvm-as -o temp.bc " ++  (dropExtension f) ++ ".ll")
        --callCommand ("llvm-link -o " ++  (dropExtension f) 
        -- ++ ".bc temp.bc lib/runtime.bc")
        --callCommand ("rm temp.bc")
        -- given too many arguments
        _ -> do
            putStrLn "Invalid number of arguments, usage: insc_jvm <filename>"

