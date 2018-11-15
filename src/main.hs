
import Data.Char

data Operator = Plus | Minus | Times | Div
        deriving (Show, Eq)

data Token = TokenOperator Operator 
            | TokenIdentifier String 
            | TokenNumber Double
            | TokenSpace
            | TokenAssign
            | TokenLeftParen
            | TokenRightParen
            | TokenEnd
        deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+'  = Plus
           | c == '-'  = Minus
           | c == '*'  = Times
           | c == '/'  = Div
           | otherwise = error "Other operator"

-- Cannot function on tokens comprised of multiple digits
-- tokenize :: String -> [Token]
-- tokenize = map tokenizeChar

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs) 
    | elem c "+-*/" = TokenOperator (operator c)    : tokenize cs
    | c == '='      = TokenAssign                   : tokenize cs
    | c == '('      = TokenLeftParen                : tokenize cs
    | c == ')'      = TokenRightParen               : tokenize cs
    | isDigit c     = number c cs
    | isAlpha c     = identifier c cs
    | isSpace c     =                                 tokenize cs
    | otherwise     =                                 error $ "Cannot tokenize " ++ [c]
               
deSpace :: [Token] -> [Token]
deSpace = filter (\t -> t /= TokenSpace)

alnums :: String -> (String, String)
alnums str = als "" str
    where
        als acc [] = (acc, [])
        als acc (c : cs) | isAlphaNum c =
                                let (acc', cs') = als acc cs
                                in (c:acc', cs')
                         | otherwise = (acc, c:cs)

identifier :: Char -> String -> [Token]
identifier c cs = let (str, cs') = span isAlphaNum cs in
                    TokenIdentifier (c : str) : tokenize cs'

-- Function: digits
-- Description: This function is the equivalent of alnums, except that it
-- gathers digits, rather than alphanumeric characters.
digits :: String -> (String, String)
digits str = digs "" str
    where
        digs :: String -> String -> (String, String)
        digs acc [] = (acc, [])
        digs acc (c : cs) | isDigit c =
                                let (acc', cs') = digs acc cs
                                in (c:acc', cs')
                          | otherwise = (acc, c:cs)

number :: Char -> String -> [Token]
number c cs = 
    let (digs, cs') = span isDigit cs in
        TokenNumber (read (c : digs)) : tokenize cs'

opToString :: Operator -> String
opToString Plus = "+"
opToString Minus = "-"
opToString Times = "*"
opToString Div = "/"

showTokenContent :: Token -> String
showTokenContent (TokenOperator op) = opToString op
showTokenContent (TokenIdentifier str) = str
showTokenContent (TokenNumber i) = show i
showTokenContent (TokenSpace) = " "
showTokenContent (TokenAssign) = "="
showTokenContent (TokenLeftParen) = "("
showTokenContent (TokenRightParen) = ")"
showTokenContent (TokenEnd) = ""

token :: Token
token = TokenIdentifier "x"

data Tree = SumNode Operator Tree Tree
          | ProductNode Operator Tree Tree
          | AssignmentNode String Tree
          | UnaryNode Operator Tree
          | NumberNode Double
          | VariableNode String
        deriving Show

-- These two functions, lookAhead and accept, are helpers for accessing tokens.
-- Traditionally, we'd want to look one token ahead, and when we want to use the
-- token, we call accept to remove it from the list, although technically
-- speaking, we're just returning the tail of the list.

lookAhead :: [Token] -> Token
lookAhead [] = TokenEnd
lookAhead (c : _) = c

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (_:ts) = ts

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

parse :: [Token] -> Tree
parse toks = let (tree, toks') = expression toks
                    in
                        if null toks'
                        then tree
                        else error $ "Leftover tokens: " ++ show toks'

--evaluate :: Expression -> Double
--evaluate = undefined


main :: IO ()
main = (print . parse . tokenize) "x1 = -15 / (2 + x2)"

-- Output:
-- AssignmentNode "x1"
--     (ProductNode Div
--         (UnaryNode Minus
--             (NumberNode 15.0))
--         (SumNode Plus
--             (NumberNode 2.0)
--             (VariableNode "x2")))
