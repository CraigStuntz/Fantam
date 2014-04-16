module Expression
    open Lexer

    type Expression = 
    | Assign      of string     * Expression
    | Call        of Expression * Expression list
    | Conditional of Expression * Expression      * Expression
    | Name        of string
    | Operator    of Expression * Token           * Expression
    | Postfix     of Expression * Token
    | Prefix      of Token      * Expression

    let rec print = function
    | Assign (name, right) -> 
        sprintf "(%s = %s)" name (print right)
    | Call   (func, args)  -> 
        sprintf "%s(%s)" (print func) (System.String.Join(", ", List.map print args))
    | Conditional (ifExp, thenExp, elseExp) -> 
        sprintf "(%s ? %s : %s)" (print ifExp) (print thenExp) (print elseExp)
    | Name   name -> 
        name
    | Operator (left, operator, right) ->
        sprintf "(%s %s %s)" (print left) (printToken operator) (print right)
    | Postfix (left, operator) ->
        sprintf "(%s%s)" (print left) (printToken operator)
    | Prefix  (operator, operand) ->
        sprintf "(%s%s)" (printToken operator) (print operand)
