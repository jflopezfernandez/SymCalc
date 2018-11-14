# SymCalc
Symbolic calculator written in Haskell.

## Description
This calculator is based on primarily on the one [written by](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/3-pure-functions-laziness-io) the man, the myth, the legend, [Bartosz Milewski](https://bartoszmilewski.com/).

At a very high level, the calculator is a loop that gets a line of text from the user and then calculates and displays the result.

The calculation is done in three steps.

1. Lexical analysis - The string is converted into tokens.
2. Parsing - Building an expression tree.
3. Evaluation - The expression is evaluated.
