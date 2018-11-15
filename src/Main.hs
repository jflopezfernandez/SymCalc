
module Main where

import qualified Data.Map as DataMap

import Lexer --(tokenize)
import Parser --(parse)
import Evaluator --(evaluate)


main = do
    loop (DataMap.fromList [("pi", pi)])

loop symbolTable = do
    str <- getLine
    if null str then
        return ()
    else
        let toks = tokenize str
            tree = parse toks
            (val, symbolTable') = evaluate tree symbolTable
        in do
            print val
            loop symbolTable'
