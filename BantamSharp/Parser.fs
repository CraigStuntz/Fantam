module Parser
    open Expression
    open Lexer


    let parse input = 
        let isPrefixToken = function
            | Plus | Minus | Bang | Tilde -> true
            | _ -> false
        let rec parseExpression = function
            | Lexer.Name name :: rest -> 
                Expression.Name name, rest
            | operator :: rest when isPrefixToken operator -> 
                let operand, rest' = parseExpression rest
                Prefix(operator, operand), rest'
            | _ -> 
                failwith "Unimplemented"
        let result, _ = parseExpression (lex input)
        result
