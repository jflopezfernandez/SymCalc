
module Main where

import qualified Data.Map as DataMap

import Lexer --(tokenize)
import Parser --(parse)
import Evaluator --(evaluate)


main = do
    loop (DataMap.fromList [("pi", pi), ("e", exp 1.0)])

loop symbolTable = do
    str <- getLine
    if null str then
        return ()
    else
        let toks = tokenize str
            tree = parse toks
            Ev ev = evaluate tree symbolTable
        in
            case ev of
                Left msg -> do
                    putStrLn $ "Error: " ++ msg
                    loop symbolTable -- Use old symbol table
                Right (v, symbolTable') -> do
                    print v
                    loop symbolTable'
