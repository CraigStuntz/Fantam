open Expression
open Parser 

let private testData = [
    // input / expected output
    ("a()", "a()")
    ("a(b)", "a(b)")
    ("a(b, c)", "a(b, c)")
    ("a(b)(c)", "a(b)(c)")
    ("a(b) + c(d)", "(a(b) + c(d))")
    ("a(b ? c : d, e + f)", "a((b ? c : d), (e + f))")
    
    // Unary precedence.
    ("~!-+a", "(~(!(-(+a))))")
    ("a!!!", "(((a!)!)!)")
    
    // Unary and binary predecence.
    ("-a * b", "((-a) * b)")
    ("!a + b", "((!a) + b)")
    ("~a ^ b", "((~a) ^ b)")
    ("-a!",    "(-(a!))")
    ("!a!",    "(!(a!))")
    
    // Binary precedence.
    ("a = b + c * d ^ e - f / g", "(a = ((b + (c * (d ^ e))) - (f / g)))")
    
    // Binary associativity.
    ("a = b = c", "(a = (b = c))")
    ("a + b - c", "((a + b) - c)")
    ("a * b / c", "((a * b) / c)")
    ("a ^ b ^ c", "(a ^ (b ^ c))")
    
    // Conditional operator.
    ("a ? b : c ? d : e", "(a ? b : (c ? d : e))")
    ("a ? b ? c : d : e", "(a ? (b ? c : d) : e)")
    ("a + b ? c * d : e / f", "((a + b) ? (c * d) : (e / f))")
    
    // Grouping.
    ("a + (b + c) + d", "((a + (b + c)) + d)")
    ("a ^ (b + c)", "(a ^ (b + c))")
    ("(!a)!",    "((!a)!)")
    ]

let private test (input, expected) = 
    try 
      let result = Parser.parse input
      let actual = print result
      match actual = expected with 
      | true  -> None
      | false -> Some (sprintf "[FAIL]   Input: %s%s         Expected: %s%s         Actual: %s" input System.Environment.NewLine expected System.Environment.NewLine actual)
    with 
      | ex    -> Some (sprintf "[FAIL]   Input: %s%s         Expected: %s%s         Error: %s" input System.Environment.NewLine expected System.Environment.NewLine ex.Message)

[<EntryPoint>]
let main argv = 
    let failed    = List.choose test testData
    let failCount = List.length failed
    List.iter (fun (failure: string) -> System.Console.WriteLine(failure)) failed
    printfn "%A tests succeeded. %A failed" (List.length testData - failCount) failCount

    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
