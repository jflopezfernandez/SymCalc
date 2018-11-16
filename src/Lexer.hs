
module Lexer (Operator(..), Token(..), tokenize, lookAhead, accept) where

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

operator :: Char -> Operator
operator c | c == '+'  = Plus
           | c == '-'  = Minus
           | c == '*'  = Times
           | c == '/'  = Div
           | otherwise = error "Other operator"

number :: Char -> String -> [Token]
number c cs = 
    let (digs, cs') = span isDigit cs in
        TokenNumber (read (c : digs)) : tokenize cs'

identifier :: Char -> String -> [Token]
identifier c cs = let (str, cs') = span isAlphaNum cs in
                    TokenIdentifier (c : str) : tokenize cs'

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
showTokenContent (TokenAssign) = "="
showTokenContent (TokenLeftParen) = "("
showTokenContent (TokenRightParen) = ")"
showTokenContent (TokenEnd) = ""
