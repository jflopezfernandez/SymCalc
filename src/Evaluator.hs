
module Evaluator (evaluate, Evaluator(..)) where

import Lexer
import Parser
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import qualified Data.Map as DataMap

type SymbolTable = DataMap.Map String Double

newtype Evaluator a = Ev (SymbolTable -> (a, SymbolTable))

instance Monad Evaluator where
    (Ev act) >>= k = Ev $
        \symbolTable ->
            let (x, symbolTable') = act symbolTable
                (Ev act') = k x
            in act' symbolTable'
    return x = Ev (\symbolTable -> (x, symbolTable))
    

instance Functor Evaluator where
    fmap = liftM

instance Applicative Evaluator where
    pure = return
    (<*>)  = ap

evaluate :: Tree -> Evaluator Double

evaluate (SumNode op left right) = do
    lft <- evaluate left
    rgt <- evaluate right
    
    case op of
        Plus ->  return $ lft + rgt
        Minus -> return $ lft - rgt


evaluate (NumberNode x) = return x
    
evaluate (ProductNode op left right) = do
    lft  <- evaluate left
    rgt  <- evaluate right
    case op of
        Times -> return $ lft * rgt
        Div   -> return $ lft / rgt

evaluate (UnaryNode op tree) = do
    x <- evaluate tree
    case op of
        Plus  -> return  x
        Minus -> return (-x)

evaluate (AssignmentNode str tree) = do
    v  <- evaluate tree
    addSymbol str v

evaluate (VariableNode str) = lookUp str


lookUp :: String -> Evaluator Double
lookUp str = Ev $ \symbolTable -> 
    case DataMap.lookup str symbolTable of
        Just v -> (v, symbolTable)
        Nothing -> error $ "Undefined variable " ++ str

addSymbol :: String -> Double -> Evaluator Double
addSymbol str val = Ev $ \symbolTable ->
    let symbolTable' = DataMap.insert str val symbolTable
    in (val, symbolTable')
