
module Parser (Tree(..), parse) where

import Lexer

data Tree = SumNode Operator Tree Tree
            | ProductNode Operator Tree Tree
            | AssignmentNode String Tree
            | UnaryNode Operator Tree
            | NumberNode Double
            | VariableNode String
        deriving Show

parse :: [Token] -> Tree
parse toks = let (tree, toks') = expression toks
                    in
                        if null toks'
                        then tree
                        else error $ "Leftover tokens: " ++ show toks'

expression :: [Token] -> (Tree, [Token])
expression toks =
    let (termTree, toks') = term toks
    in
        case lookAhead toks' of
            -- Term [+-] Expression
            (TokenOperator op) | elem op [Plus, Minus] ->
                let (exTree, toks'') = expression (accept toks')
                in (SumNode op termTree exTree, toks'')
            -- Identifier '=' Expression
            TokenAssign ->
                case termTree of
                    VariableNode str ->
                        let (exTree, toks'') = expression (accept toks')
                        in (AssignmentNode str exTree, toks'')
                    _ -> error "Only variables can be assigned to"
            -- Term
            _ -> (termTree, toks')

term :: [Token] -> (Tree, [Token])
term toks =
    let (facTree, toks') = factor toks
    in
        case lookAhead toks' of
            (TokenOperator op) | elem op [Times, Div] ->
                let (termTree, toks'') = term (accept toks')
                in (ProductNode op facTree termTree, toks'')
            _ -> (facTree, toks')

factor :: [Token] -> (Tree, [Token])
factor toks =
    case lookAhead toks of
        (TokenNumber x) -> (NumberNode x, accept toks)
        (TokenIdentifier str) -> (VariableNode str, accept toks)
        (TokenOperator op) | elem op [Plus, Minus] ->
            let (facTree, toks') = factor (accept toks)
            in (UnaryNode op facTree, toks')
        TokenLeftParen ->
            let (expTree, toks') = expression (accept toks)
            in 
                if lookAhead toks' /= TokenRightParen
                then error "Missing right parenthesis"
                else (expTree, accept toks')
        _ -> error $ "Parse error on token: " ++ show toks
