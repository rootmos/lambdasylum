open Core_kernel.Std

let () =
  Errors.run_with_pretty_errors ~err:(fun _ -> exit 1) (fun () ->
    let f s = s
      |> Parse_utils.parse
      |> Ulambda.church
      |> Ulambda.reduce Ulambda.Ctx.predef
      |> Ulambda.sexp_of_value
      |> Sexp.to_string_hum
      |> print_endline in
    f "1";
    f "1+2";
    f "(λx.x) 7";
    f "λy.((λx.x) 7)";
    f "λy.(λx.x+y) 7";
    f "(λ_. 1) 2";
  )
