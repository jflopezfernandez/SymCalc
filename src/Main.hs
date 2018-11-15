
module Main where

    import Lexer

    import qualified Data.Map

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

    data Tree = SumNode Operator Tree Tree
            | ProductNode Operator Tree Tree
            | AssignmentNode String Tree
            | UnaryNode Operator Tree
            | NumberNode Double
            | VariableNode String
            deriving Show



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

    type SymbolTable = Data.Map.Map String Double

    lookUp :: String -> SymbolTable -> (Double, SymbolTable)
    lookUp str symbolTable =
        case Data.Map.lookup str symbolTable of
            Just v -> (v, symbolTable)
            Nothing -> error $ "Undefined variable " ++ str

    addSymbol :: String -> Double -> SymbolTable -> ((), SymbolTable)
    addSymbol str val symbolTable =
        let symbolTable' = Data.Map.insert str val symbolTable
        in ((), symbolTable')

    evaluate :: Tree -> SymbolTable -> (a, SymbolTable)
    evaluate (NumberNode x) symbolTable = (x, symbolTable)

    evaluate (SumNode op left right) symbolTable =
        let (lft, symbolTable') = evaluate left symbolTable
            (rgt, symbolTable'') = evaluate right symbolTable'
        in
            case op of
                Plus -> (lft + rgt, symbolTable'')
                Minus -> (lft - rgt, symbolTable'')

    evaluate (ProductNode op left right) =
        let lft = evaluate left
            rgt = evaluate right
        in
            case op of
                Times -> lft * rgt
                Div -> lft / rgt
                -- Pattern matches weren't exhaustive
                _ -> 1

    evaluate (UnaryNode op tree) symbolTable =
        let x = evaluate tree
        in case op of
            Plus -> (x, symbolTable')
            Minus -> -x
            -- Pattern matches weren't exhaustive
            _ -> x

    -- Dummy Implementation
    evaluate (AssignmentNode str tree) symbolTable =
        let (v, symbolTable') = evaluate tree symbolTable
            (_, symbolTable'') = addSymbol str v symbolTable'
        in (v, symbolTable'')

    -- Dummy Implementation
    evaluate (VariableNode str symbolTable) = lookUp str symbolTable


    main = do
        loop (Data.Map.fromList [("pi", pi)])

    loop symbolTable = do
        str <- getLine
        if null str then
            return ()
        else
            let toks = tokenize str
                tree = parse toks
                (val, symbolTable') = evaluate tree symbolTable
            in do
                --print val
                loop symbolTable'

    --main :: IO ()
    --main = (print . parse . tokenize) "x1 = -15 / (2 + x2)"

    -- Output:
    -- AssignmentNode "x1"
    --     (ProductNode Div
    --         (UnaryNode Minus
    --             (NumberNode 15.0))
    --         (SumNode Plus
    --             (NumberNode 2.0)
    --             (VariableNode "x2")))
