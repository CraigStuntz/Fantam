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
                Expression.Assign(name, right), rest
            | _ -> failwith "The left-hand side of an assignment must be a name."
        and parseCall left tokens = 
            let args, rest = parseCallArgs [] tokens
            Call(left, args), rest
        and parseConditional left tokens = 
            let thenArm, rest = parseExpression 0 tokens
            match rest with
            | Colon :: rest' ->
                let elseArm, rest'' = parseExpression ((infixPrecedence Question) - 1) rest'
                Conditional(left, thenArm, elseArm), rest''
            | _              -> failwith "Expected ':'."
        and parseInfixOperator operator left tokens = 
            let precedence = 
                match isRightAssociative operator with
                | true  -> (infixPrecedence operator) - 1
                | false -> (infixPrecedence operator) 
            let right, rest = parseExpression precedence tokens
            Operator(left, operator, right), rest
        and parsePostfixOperator operator left tokens = 
            Postfix(left, operator), tokens
        and infixParserForToken = function 
            | Assign                           -> Some parseAssign
            | LeftParen                        -> Some parseCall
            | Question                         -> Some parseConditional
            | operator when isInfix operator   -> Some (parseInfixOperator operator )
            | operator when isPostfix operator -> Some (parsePostfixOperator operator)
            | _                                -> None
        and parseInfix precedence left tokens = 
            match tokens with 
            | []            -> left, tokens
            | token :: rest ->
                if precedence >= (infixPrecedence token) 
                then left, tokens
                else
                    match infixParserForToken token with
                    | Some parselet -> 
                        let left', rest' = parselet left rest
                        parseInfix precedence left' rest'
                    | None -> left, rest
        and parseExpression precedence tokens = 
            let left, rest = parsePrefix tokens
            parseInfix precedence left rest 

        let result, _ = parseExpression 0 (lex input)
        result