module Parser
    open Expression
    open Lexer

    type private Precedence private () = 
    // Ordered in increasing precedence.
        static member None        = 0
        static member Assignment  = 1
        static member Conditional = 2
        static member Sum         = 3
        static member Product     = 4
        static member Exponent    = 5
        static member Prefix      = 6
        static member Postfix     = 7
        static member Call        = 8
    
    let parse input = 
        let oneOf tokens token = List.exists ((=) token) tokens
        let isInfix            = oneOf [ Plus; Minus; Asterisk; Slash; Caret ]
        let isPrefix           = oneOf [ Plus; Minus; Bang; Tilde ]
        let isPostfix          = oneOf [ Bang ]
        let isRightAssociative = oneOf [ Caret ]
        let infixPrecedence = function 
            | Name _ | RightParen 
            | Colon  | Comma 
            | Tilde  | Eof     -> Precedence.None
            | Assign           -> Precedence.Assignment
            | Question         -> Precedence.Conditional
            | Plus     | Minus -> Precedence.Sum
            | Asterisk | Slash -> Precedence.Product
            | Caret            -> Precedence.Exponent
            | Bang             -> Precedence.Postfix
            | LeftParen        -> Precedence.Call

        let rec parseCallArgs accum = function 
            | RightParen :: rest -> accum, rest
            | rest               -> 
                match parseExpression Precedence.None rest with 
                | arg, RightParen :: rest' -> accum @ [ arg ] , rest'
                | arg, Comma      :: rest' -> parseCallArgs (accum @ [ arg ]) rest'
                | _                        -> [Error "Expected ',' or ')'"], rest
        and parsePrefix = function
            | Lexer.Name name :: rest -> 
                Expression.Name name, rest
            | LeftParen :: rest ->
                match parseExpression Precedence.None rest with 
                | expression, RightParen :: rest' -> expression, rest'
                | _                               -> Error "Expected ')'", rest
            | operator :: rest when isPrefix operator -> 
                let operand, rest' = parseExpression Precedence.Prefix rest
                Prefix(operator, operand), rest'
            | _ -> 
                Error "Unimplemented", []
        and parseAssign left tokens = 
            match left with 
            | Expression.Name name -> 
                let right, rest = parseExpression (Precedence.Assignment - 1) tokens
                Expression.Assign(name, right), rest
            | _ -> Error "The left-hand side of an assignment must be a name.", tokens
        and parseCall left tokens = 
            let args, rest = parseCallArgs [] tokens
            Call(left, args), rest
        and parseConditional left tokens = 
            match parseExpression Precedence.None tokens with
            | thenArm, Colon :: rest ->
                let elseArm, rest' = parseExpression (Precedence.Conditional - 1) rest
                Conditional(left, thenArm, elseArm), rest'
            | _                      -> Error "Expected ':'.", tokens
        and parseInfixOperator operator left tokens = 
            let precedence = 
                match isRightAssociative operator with
                | true  -> (infixPrecedence operator) - 1
                | false -> (infixPrecedence operator) 
            let right, rest = parseExpression precedence tokens
            Operator(left, operator, right), rest
        and parsePostfixOperator operator left tokens = 
            Postfix(left, operator), tokens
        and parseInfix precedence left tokens = 
            let infixParserForToken = function 
                | Assign                           -> Some parseAssign
                | LeftParen                        -> Some parseCall
                | Question                         -> Some parseConditional
                | operator when isInfix operator   -> Some (parseInfixOperator operator )
                | operator when isPostfix operator -> Some (parsePostfixOperator operator)
                | _                                -> None
            match tokens with 
            | token :: rest when precedence < (infixPrecedence token) ->
                match infixParserForToken token with
                | Some parser -> 
                    let left', rest' = parser left rest
                    parseInfix precedence left' rest'
                | None -> left, rest
            | _ -> left, tokens
        and parseExpression precedence tokens = 
            let left, rest = parsePrefix tokens
            parseInfix precedence left rest 

        let result, _ = parseExpression Precedence.None (lex input)
        result