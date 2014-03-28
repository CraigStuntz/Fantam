module Parser
    open Expression
    open Lexer

    let private isPrefixToken = function
        | Plus | Minus | Bang | Tilde -> true
        | _ -> false

    let rec parse = function
        | Lexer.Name name :: rest -> Expression.Name name, rest
        | operator :: rest when isPrefixToken operator -> 
            let operand, rest' = parse rest
            Prefix(operator, operand), rest'
        | _ -> failwith "Unimplemented"

