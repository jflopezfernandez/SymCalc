@ECHO OFF

CLS

ghc -o calc.exe Lexer.hs Parser.hs Evaluator.hs Main.hs
