
module Evaluator (evaluate, Evaluator(..)) where

import Lexer
import Parser
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import qualified Data.Map as DataMap

type SymbolTable = DataMap.Map String Double

newtype Evaluator a = Ev (Either String a)

instance Monad Evaluator where
    (Ev ev) >>= k =
        case ev of
            Left msg -> Ev (Left msg)
            Right v -> k v
    --return v = Ev (Right v)
    fail msg = Ev (Left msg)

instance Functor Evaluator where
    fmap = liftM

instance Applicative Evaluator where
    pure v = Ev (Right v)
    (<*>) = ap


evaluate :: Tree -> SymbolTable -> Evaluator (Double, SymbolTable)

evaluate (SumNode op left right) symbolTable = do
    (lft, symbolTable')  <- evaluate left symbolTable
    (rgt, symbolTable'') <- evaluate right symbolTable'
    
    case op of
        Plus ->  return (lft + rgt, symbolTable'') -- was return
        Minus -> return (lft - rgt, symbolTable'') -- was return


evaluate (NumberNode x) symbolTable = return (x, symbolTable)
    
evaluate (ProductNode op left right) symbolTable = do
    (lft, symbolTable')  <- evaluate left symbolTable
    (rgt, symbolTable'') <- evaluate right symbolTable'
    case op of
        Times -> return (lft * rgt, symbolTable)
        Div   -> return (lft / rgt, symbolTable)

evaluate (UnaryNode op tree) symbolTable = do
    (x, symbolTable') <- evaluate tree symbolTable
    case op of
        Plus ->  return ( x, symbolTable')
        Minus -> return (-x, symbolTable')

evaluate (AssignmentNode str tree) symbolTable = do
    (v, symbolTable')  <- evaluate tree symbolTable
    (_, symbolTable'') <- addSymbol str v symbolTable'
    return (v, symbolTable'')


evaluate (VariableNode str) symbolTable = lookUp str symbolTable


lookUp :: String -> SymbolTable -> Evaluator (Double, SymbolTable)
lookUp str symbolTable =
    case DataMap.lookup str symbolTable of
        Just v -> return (v, symbolTable)
        Nothing -> fail ("Undefined variable " ++ str)

addSymbol :: String -> Double -> SymbolTable -> Evaluator ((), SymbolTable)
addSymbol str val symbolTable =
    let symbolTable' = DataMap.insert str val symbolTable
    in return ((), symbolTable')
