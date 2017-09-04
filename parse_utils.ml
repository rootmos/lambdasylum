type error = Parsing | Lexing of string
exception Parse_utils_exception of error

let parse s =
  try Lexing.from_string s |> Parser.program Lexer.read with
  | Parser.Error -> raise @@ Parse_utils_exception Parsing
  | Lexer.Syntax_error msg -> raise @@ Parse_utils_exception (Lexing msg)
