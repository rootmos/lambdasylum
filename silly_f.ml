open Core_kernel.Std

let () =
  Errors.run_with_pretty_errors ~err:(fun _ -> exit 1) (fun () ->
    let c s = s
      |> Parse_utils.parse
      |> Ulambda.church
      |> Ulambda.reduce Ulambda.Ctx_term.empty in
    let p ul = ul |> Ulambda.pretty |> print_endline in
    let i ul = ul
      |> Ulambda.unchurch_int
      |> Pervasives.print_int;
      Pervasives.print_newline ()
    in
    c "1" |> i;
    c "1+2" |> i;
    c "(λx.x) 7" |> i;
    c "λy.((λx.x) 7)" |> p;
    c "λy.(λx.x+y) 7" |> p;
    c "(λ_. 1) 2" |> i;
  )
