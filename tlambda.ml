open Printf

type error =
  Lexing of string
| Parsing
exception Tlambda_exception of error

let parse s =
  try Lexing.from_string s |> Tlambda_parser.program Lexer.read with
  | Tlambda_parser.Error -> raise @@ Tlambda_exception Parsing
  | Lexer.Syntax_error msg -> raise @@ Tlambda_exception (Lexing msg)

let explain = function
  Parsing -> sprintf "parsing error"
| Lexing s -> sprintf "lexing error: %s" s
