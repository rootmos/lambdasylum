open Core_kernel.Std
open Printf

type calculus = {
  name: string;
  compile: string -> Ulambda.value
}

let rec repl c () =
  printf "%s> " c.name;
  Out_channel.flush Out_channel.stdout;
  Errors.run_with_pretty_errors ~err:(fun _ ->
    Out_channel.flush Out_channel.stderr;
    repl c ()
  ) (fun () ->
    match In_channel.input_line In_channel.stdin with
    | Some l ->
        c.compile l
          |> Ulambda.Church.unchurch
          |> Ulambda.Church.pretty
          |> print_endline;
        Out_channel.flush Out_channel.stdout;
        repl c ()
    | None -> ()
  )

let () =
  print_endline "Welcome to the Lambdasylum!";
  let c = { name = "ulambda"; compile = Ulambda.compile } in
  repl c ()
