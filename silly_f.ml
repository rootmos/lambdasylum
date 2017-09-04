open Core_kernel.Std

let () =
  let p = Parse_utils.parse "λy.λx.x" in
  Parsed.sexp_of_program p |> Sexp.to_string_hum |> print_endline
