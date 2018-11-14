
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

data Expression

expression :: [Token] -> (Tree, [Token])
expression = undefined

term :: [Token] -> (Tree, [Token])
term = undefined

factor :: [Token] -> (Tree, [Token])
factor = undefined

parse :: [Token] -> Expression
parse = undefined

evaluate :: Expression -> Double
evaluate = undefined


main :: IO ()
main = do
    print $ tokenize " 1 + 4 / x"
    putStrLn ""
    print $ tokenize "2+2+2*x+1"
    putStrLn ""
    print $ tokenize "12 + 24 / x1"
    putStrLn ""
