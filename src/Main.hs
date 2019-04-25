module Main where

import qualified Lexer as L
import Parser
import System.Environment
import System.Exit
import QBF
import Picosat
import Solver

main = do
    args <- getArgs
    res <- parse args
    test res

parse ["-h"] = putStrLn "Usage: hasqbf [-vh] <file>" >> exitWith ExitSuccess
parse ["-v"] = putStrLn "hasqbf 0.1" >> exitWith ExitSuccess
parse [] =  putStrLn "Usage: hasqbf [-vh] <file>" >> exitWith ExitSuccess
parse (f:[]) = return f
parse (f:fs) = do
                putStrLn $ "I received more than one file name. I will now continue to solve only the first file supplied (" ++ f ++ ")" 
                return f

test path = do
    input <- readFile path    
    let problem = parseQDIMACS $ L.alexScanTokens input       
    putStrLn "Problem: "    
    print problem
    --putStr "Pure literals: "
    --print $ findPureLiterals $ q
    --putStrLn "After pureLiteralElimination: "
    let ple = pureLiteralElimination problem
    --print ple
    --putStr "Unit literals: "
    --print  $ findUnitLiterals $ q
    --putStrLn "After unit literal elimination: "
    let ule = unitLiteralElimination ple    
    putStrLn "Result:"
    solution <- expansionSolve ule
    print solution
