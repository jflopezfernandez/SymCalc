
# Calculator Grammar

This is the grammar used by the calculator in order to parse, lex, and evaluate the input.

## Expressions

We expect the result of parsing a line of user input to be an `Expression`.

1. An additive `Expression` that starts with a term followed by either plus or minus, followed by another `Expression`. This is a typical recursive definition for parsing `Expressions` of the type `Term + Term - Term ... etc`. Example: `x - 5 + y`.

2. An assignment of the form: `Identifier = Expression`. For simplicity, we will treat an assignment as an `Expression`, as it is in C++, rather than a separate statement. Example: `x = 2 + 2`.

3. Finally, a lonely `Term` is also considered an `Expression`. Example: `44`.

## Terms

`Terms` are more tightly bound that `Expressions`, corresponding to a higher precedence of multiplicative vs. additive operators. We'll consider two forms of `Terms`:

1. `Factor` followed by a multiplication or division sign, followed by another `Term`. This production corresponds to `Terms` of the form `Factor * Factor / Factor ...`. Example: `2 * x / 2`.

2. A `Term` could also be a lonely `Factor`. Example: `44`.

## Factor

1. A `Number`, like `147`.

2. An `Identifier`, i.e... a variable name, like `x1`.

3. A unary plus or minus in front of a `Factor`, like `-x` or `+12`.

4. A parenthized `Expression` like `(a + b/2)`.

## Grammar in Backus Normal Form

```
Expression := Term [+-] Expression
           |  Identifier '=' Expression
           |  Term

Term       := Factor [*/] Term
           |  Factor

Factor     := Number
           |  Identifier
           |  [+-] Factor
           |  '(' Expression ')'
```

> Note: the associativity of the grammar is wrong; operators of the same precedence associate to the right rather than to the left, so for instance `5 - 3 + 2` is interpreted as `5 - (3 + 2)`. Correcting the grammar to the proper left-associative form is on the To Do list.

# The Parse Tree

The productions of this grammar map nicely into the structure of a tree; the leaf nodes are either variables or numbers. Expressions generate binary addition/subtraction nodes. Terms produce binary multiplication/division nodes. There are also nodes for unary plus or minus, and the assignment node. We end up with this tree-like recursive data structure:

```
data Tree = SumNode Operator Tree Tree
          | ProductNode Operator Tree Tree
          | AssignmentNode String Tree
          | UnaryNode Operator Tree
          | NumberNode Double
          | VariableNode String
        deriving Show
```
