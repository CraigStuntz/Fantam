module Lexer
    type Token = 
    | LeftParen
    | RightParen
    | Comma
    | Assign
    | Plus
    | Minus
    | Asterisk
    | Slash
    | Caret
    | Tilde
    | Bang
    | Question
    | Colon
    | Name of string
    | Eof

    let printToken = function
    | LeftParen   -> "("
    | RightParen  -> ")"
    | Comma       -> ","
    | Assign      -> "="
    | Plus        -> "+"
    | Minus       -> "-"
    | Asterisk    -> "*"
    | Slash       -> "/"
    | Caret       -> "^"
    | Tilde       -> "~"
    | Bang        -> "!"
    | Question    -> "?"
    | Colon       -> ":"
    | Name name   -> name
    | Eof         -> ""