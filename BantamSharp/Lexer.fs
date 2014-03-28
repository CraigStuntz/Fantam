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

    let rec lex = function 
        | '(' :: rest -> LeftParen :: lex(rest)
        | ')' :: rest -> RightParen :: lex(rest)
        | ',' :: rest -> Comma :: lex(rest)
        | '=' :: rest -> Assign :: lex(rest)
        | '+' :: rest -> Plus :: lex(rest)
        | '-' :: rest -> Minus :: lex(rest)
        | '*' :: rest -> Asterisk :: lex(rest)
        | '/' :: rest -> Slash :: lex(rest)
        | '^' :: rest -> Caret :: lex(rest)
        | '~' :: rest -> Tilde :: lex(rest)
        | '!' :: rest -> Bang :: lex(rest)
        | '?' :: rest -> Question :: lex(rest)
        | ':' :: rest -> Colon :: lex(rest)
        | []          -> [ Eof ]
        | c :: rest when Char.IsLetter(c) -> lexName(rest, [c])
        | _ :: rest   -> lex(rest) // Ignore anything else
    and lexName(source: char list, name: char list) = 
        match source with 
        | c :: rest when Char.IsLetter(c) -> lexName(rest, name @ [c])
        | _ -> Name(string(name)) :: lex(source)

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