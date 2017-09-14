open Core_kernel.Std
open Printf
open Out_channel
open In_channel

type calculus = {
  name: string;
  compile: string -> Ulambda.value
}

let help () =
  printf "\\lambda (or \\l<TAB>) expands to λ\n";
  printf "\\Lambda (or \\L<TAB>) expands to Λ\n";
  printf "\\forall (or \\f<TAB>) expands to ∀\n";
  printf "\\bottom (or \\b<TAB>) expands to ⊥\n";
  newline stdout;
  printf ":ulambda  switch to untyped lambda calculus\n";
  printf ":tlambda  switch to simply typed lambda calculs\n";
  printf ":flambda  switch to System F\n"

let ulambda = { name = "ulambda"; compile = Ulambda.compile }
let tlambda = { name = "tlambda"; compile = Tlambda.compile }
let flambda = { name = "flambda"; compile = Flambda.compile }

let rec repl c () =
  printf "%s> " c.name;
  flush stdout;
  let err _ = flush stderr; repl c () in
  Errors.run_with_pretty_errors ~err (fun () ->
    match input_line stdin with
    | Some l when String.is_prefix l ":h" -> help (); repl c ()
    | Some l when String.is_prefix l ":u" -> repl ulambda ()
    | Some l when String.is_prefix l ":t" -> repl tlambda ()
    | Some l when String.is_prefix l ":f" -> repl flambda ()
    | Some l ->
        c.compile l
          |> Ulambda.Church.unchurch
          |> Ulambda.Church.pretty
          |> print_endline;
        flush stdout;
        repl c ()
    | None -> ()
  )

let welcome () =
  printf "Welcome to the Lambdasylum!\n";
  printf "  enter :help to get some help\n"

let () =
  welcome ();
  repl ulambda ()
