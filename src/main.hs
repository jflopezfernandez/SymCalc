
import Data.Char

data Operator = Plus | Minus | Times | Div
        deriving (Show, Eq)

data Token = TokenOperator Operator 
            | TokenIdentifier String 
            | TokenNumber Int
            | TokenSpace
        deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+'  = Plus
           | c == '-'  = Minus
           | c == '*'  = Times
           | c == '/'  = Div
           | otherwise = error "Other operator"

tokenize :: String -> [Token]
tokenize = map tokenizeChar

tokenizeChar :: Char -> Token
tokenizeChar c | elem c "+-*/" = TokenOperator (operator c)
               | isDigit c     = TokenNumber (digitToInt c)
               | isAlpha c     = TokenIdentifier [c]
               | isSpace c     = TokenSpace
               | otherwise     = error $ "Cannot tokenize " ++ [c]
               
deSpace :: [Token] -> [Token]
deSpace = filter (\t -> t /= TokenSpace)

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

token :: Token
token = TokenIdentifier "x"

data Expression

parse :: [Token] -> Expression
parse = undefined

evaluate :: Expression -> Double
evaluate = undefined


main :: IO ()
main = do
    print $ tokenize " 1 + 4 / x"
    print $ tokenize "2+2+2*x+1"
