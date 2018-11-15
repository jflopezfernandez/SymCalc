
module Evaluator (evaluate) where

import Lexer
import Parser
import qualified Data.Map

type SymbolTable = Data.Map.Map String Double

evaluate :: Tree -> SymbolTable -> (a, SymbolTable)
evaluate (NumberNode x) symbolTable = (x, symbolTable)

evaluate (SumNode op left right) symbolTable =
    let (lft, symbolTable') = evaluate left symbolTable
        (rgt, symbolTable'') = evaluate right symbolTable'
    in
        case op of
            Plus -> (lft + rgt, symbolTable'')
            Minus -> (lft - rgt, symbolTable'')

evaluate (ProductNode op left right) =
    let lft = evaluate left
        rgt = evaluate right
    in
        case op of
            Times -> lft * rgt
            Div -> lft / rgt
            -- Pattern matches weren't exhaustive
            _ -> 1

evaluate (UnaryNode op tree) symbolTable =
    let x = evaluate tree
    in case op of
        Plus -> (x, symbolTable')
        Minus -> -x
        -- Pattern matches weren't exhaustive
        _ -> x

-- Dummy Implementation
evaluate (AssignmentNode str tree) symbolTable =
    let (v, symbolTable') = evaluate tree symbolTable
        (_, symbolTable'') = addSymbol str v symbolTable'
    in (v, symbolTable'')

-- Dummy Implementation
evaluate (VariableNode str symbolTable) = lookUp str symbolTable

lookUp :: String -> SymbolTable -> (Double, SymbolTable)
lookUp str symbolTable =
    case Data.Map.lookup str symbolTable of
        Just v -> (v, symbolTable)
        Nothing -> error $ "Undefined variable " ++ str

addSymbol :: String -> Double -> SymbolTable -> ((), SymbolTable)
addSymbol str val symbolTable =
    let symbolTable' = Data.Map.insert str val symbolTable
    in ((), symbolTable')
