open Core_kernel.Std

let () =
  Errors.run_with_pretty_errors (fun () ->
    let f s = s
      |> Parse_utils.parse
      |> Parsed.sexp_of_program
      |> Sexp.to_string_hum
      |> print_endline in
    f "λy.((λx.x) 7)";
    f "λy.(λx.x+y) 7";
  )
