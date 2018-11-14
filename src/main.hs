
import System.Environment
import Data.Char


data Operator = Plus | Minus | Times | Div
        deriving (Show, Eq)

operatorToString :: Operator -> String
operatorToString Plus = "+"
operatorToString Minus = "-"
operatorToString Times = "*"
operatorToString Div = "/"

data Token = TokenOperator Operator
           | TokenIdentifier String
           | TokenNumber Int
        deriving (Show, Eq)

showContent :: Token -> String
showContent (TokenOperator op) = operatorToString
showContent (TokenIdentifier str) = operatorToString
showContent (TokenNumber i) = show i

main :: IO ()
main = do
    print $ operatorToCharacter Plus
    print $ operatorToCharacter Minus
    print $ operatorToCharacter Times
    print $ operatorToCharacter Div
