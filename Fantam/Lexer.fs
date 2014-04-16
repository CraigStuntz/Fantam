module Lexer
    open System

    type Token =
    | LeftParen | RightParen
    | Comma
    | Assign
    | Plus | Minus | Asterisk | Slash
    | Caret
    | Tilde
    | Bang
    | Question | Colon
    | Name of string
    | Eof

    let lex input = 
        let rec lexChars = function 
            | '(' :: rest -> LeftParen  :: lexChars rest
            | ')' :: rest -> RightParen :: lexChars rest
            | ',' :: rest -> Comma      :: lexChars rest
            | '=' :: rest -> Assign     :: lexChars rest
            | '+' :: rest -> Plus       :: lexChars rest
            | '-' :: rest -> Minus      :: lexChars rest
            | '*' :: rest -> Asterisk   :: lexChars rest
            | '/' :: rest -> Slash      :: lexChars rest
            | '^' :: rest -> Caret      :: lexChars rest
            | '~' :: rest -> Tilde      :: lexChars rest
            | '!' :: rest -> Bang       :: lexChars rest
            | '?' :: rest -> Question   :: lexChars rest
            | ':' :: rest -> Colon      :: lexChars rest
            | []          -> [ Eof ]
            | c :: rest when Char.IsLetter(c) -> lexName(rest, [c])
            | _ :: rest                       -> lexChars rest // Ignore anything else
        and lexName(source: char list, name: char list) = 
            match source with 
            | c :: rest when Char.IsLetter(c) -> lexName(rest, name @ [c])
            | _ -> Name(new System.String(name |> Array.ofList)) :: lexChars(source)
        lexChars (List.ofSeq input)

    let printToken = function
    | LeftParen  -> "("
    | RightParen -> ")"
    | Comma      -> ","
    | Assign     -> "="
    | Plus       -> "+"
    | Minus      -> "-" 
    | Asterisk   -> "*"
    | Slash      -> "/"
    | Caret      -> "^"
    | Tilde      -> "~"
    | Bang       -> "!"
    | Question   -> "?"
    | Colon      -> ":"
    | Name(n)    -> n
    | Eof        -> ""