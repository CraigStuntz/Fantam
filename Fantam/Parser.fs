module Parser
    open Expression
    open Lexer

    let parse input = 
        let oneOf tokens token = List.exists ((=) token) tokens
        let isInfix            = oneOf [ Plus; Minus; Asterisk; Slash; Caret ]
        let isPrefix           = oneOf [ Plus; Minus; Bang; Tilde ]
        let isPostfix          = oneOf [ Bang ]
        let isRightAssociative = oneOf [ Caret ]
        let infixPrecedence = function 
            | Assign           -> 1
            | Question         -> 2
            | Plus | Minus     -> 3
            | Asterisk | Slash -> 4
            | Caret            -> 5
            | Bang             -> 7
            | LeftParen        -> 8
            | _                -> 0
        let rec parseCallArgs accum = function 
            | RightParen :: rest -> accum, rest
            | rest               -> 
                let arg, rest' = parseExpression 0 rest
                match rest' with 
                | RightParen :: rest'' -> accum @ [ arg ] , rest''
                | Comma      :: rest'' -> parseCallArgs (accum @ [ arg ]) rest''
                | _                    -> failwith "Expected ',' or ')'"
        and parsePrefix = function
            | Lexer.Name name :: rest -> 
                Expression.Name name, rest
            | LeftParen :: rest ->
                let expression, rest' = parseExpression 0 rest
                match rest' with 
                | RightParen :: rest'' -> expression, rest''
                | _                    -> failwith "Expected ')'"
            | operator :: rest when isPrefix operator -> 
                let operand, rest' = parseExpression 6 rest
                Prefix(operator, operand), rest'
            | _ -> 
                failwith "Unimplemented"
        and parseAssign left tokens = 
            match left with 
            | Expression.Name name -> 
                let right, rest = parseExpression ((infixPrecedence Assign) - 1)  tokens
                Some(Expression.Assign(name, right), rest)
            | _ -> failwith "The left-hand side of an assignment must be a name."
        and parseCall left tokens = 
            let args, rest = parseCallArgs [] tokens
            Some(Call(left, args), rest)
        and parseConditional left tokens = 
            let thenArm, rest = parseExpression 0 tokens
            match rest with
            | Colon :: rest' ->
                let elseArm, rest'' = parseExpression ((infixPrecedence Question) - 1) rest'
                Some(Conditional(left, thenArm, elseArm), rest'')
            | _               -> failwith "Expected ':'."
        and parseInfixOperator operator left tokens = 
            let precedence' = 
                match isRightAssociative operator with
                | true  -> (infixPrecedence operator) - 1
                | false -> (infixPrecedence operator) 
            let right, rest' = parseExpression precedence' tokens
            Some(Operator(left, operator, right), rest')
        and parsePostfixOperator operator left tokens = 
            Some(Postfix(left, operator), tokens)
        and parseInfix precedence left tokens = 
            match tokens with 
            | token :: rest ->
                if precedence < (infixPrecedence token) 
                then
                    let parsed =   
                        match token with 
                        | Assign                           -> parseAssign left rest
                        | LeftParen                        -> parseCall left rest
                        | Question                         -> parseConditional left rest
                        | operator when isInfix operator   -> parseInfixOperator operator left rest
                        | operator when isPostfix operator -> parsePostfixOperator operator left rest
                        | _ -> None 
                    match parsed with
                    | Some(left', rest') -> parseInfix precedence left' rest'
                    | None               -> Some(left, rest)
                else Some(left, tokens)
            | _ -> None
        and parseExpression precedence tokens = 
            let left, rest = parsePrefix tokens
            match parseInfix precedence left rest with 
            | Some (expr, rest') -> expr, rest'
            | None               -> left, rest 

        let result, _ = parseExpression 0 (lex input)
        result
