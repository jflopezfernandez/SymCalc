
module Main where

import qualified Data.Map as DataMap

import Lexer --(tokenize)
import Parser --(parse)
import Evaluator --(evaluate)


main = do
    print "Calc > "
    loop (DataMap.fromList [("pi", pi), ("e", exp 1.0)])

loop symbolTable = do
    str <- getLine
    if null str then
        return ()
    else
        let toks = tokenize str
            tree = parse toks
            Ev act = evaluate tree
            (val, symbolTable') = act symbolTable
        in do
            print val
            loop symbolTable'
