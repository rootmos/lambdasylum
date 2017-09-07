open Core_kernel.Std
open Printf

let () =
  Errors.run_with_pretty_errors ~err:(fun _ -> exit 1) (fun () ->
    let c s = s
      |> Parse_utils.parse
      |> Ulambda.church
      |> Ulambda.reduce Ulambda.predef in
    let p ul = ul |> Ulambda.pretty |> print_endline in
    let i = Ulambda.unchurch_int
    in

    let tcs = [
      "1", 1;
      "1+2", 3;

      "if true 1 2", 1;
      "if false 1 2", 2;

      "if (and true true) 1 2", 1;
      "if (and false true) 1 2", 2;
      "if (and true false) 1 2", 2;
      "if (and false false) 1 2", 2;

      "if (or true true) 1 2", 1;
      "if (or false true) 1 2", 1;
      "if (or true false) 1 2", 1;
      "if (or false false) 1 2", 2;

      "if (zero? 0) 1 2", 1;
      "if (zero? 1) 1 2", 2;
      "if (zero? 7) 1 2", 2;
    ] in
    List.iter tcs ~f:(fun (s, j) ->
      printf "reducing: %s " s;
      let j' = c s |> i in
      printf "->* %d (expected %d)\n" j' j;
      assert (j = j')
    )
  )
