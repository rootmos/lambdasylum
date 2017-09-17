open Core_kernel.Std
open Printf
open Out_channel
open In_channel

type calculus = {
  name: string;
  compile: string -> Clambda.value;
  type_of: (string -> string) option;
}

let help () =
  printf "\\lambda (or \\l<TAB>) expands to λ\n";
  printf "\\Lambda (or \\L<TAB>) expands to Λ\n";
  printf "\\forall (or \\f<TAB>) expands to ∀\n";
  printf "\\bottom (or \\b<TAB>) expands to ⊥\n";
  newline stdout;
  printf ":clambda  switch to core lambda calculus\n";
  printf ":ulambda  switch to untyped lambda calculus with Church encodings\n";
  printf ":tlambda  switch to simply typed lambda calculus\n";
  printf ":flambda  switch to System F\n";
  printf ":tilambda switch to a type-inferred lambda calculus (Hindley–Milner style)\n";
  newline stdout;
  printf ":type <expr>  to check the type of <expr>\n";
  printf ":help         to show this message\n";
  ()

let clambda = { name = "clambda"; compile = Clambda.compile; type_of = None }
let ulambda = { name = "ulambda"; compile = Ulambda.compile; type_of = None }
let tlambda = { name = "tlambda"; compile = Tlambda.compile; type_of = None }
let flambda = { name = "flambda"; compile = Flambda.compile; type_of = None }
let tilambda = {
  name = "tilambda";
  compile = Tilambda.compile;
  type_of = Some Tilambda.type_of
}

let rec repl c () =
  printf "%s> " c.name;
  flush stdout;
  let err _ = flush stderr; repl c () in
  Errors.run_with_pretty_errors ~err (fun () ->
    match input_line stdin with
    | Some l when String.is_prefix l ":h" -> help (); repl c ()
    | Some l when String.is_prefix l ":c" -> repl clambda ()
    | Some l when String.is_prefix l ":u" -> repl ulambda ()
    | Some l when String.is_prefix l ":tl" -> repl tlambda ()
    | Some l when String.is_prefix l ":ti" -> repl tilambda ()
    | Some l when String.is_prefix l ":f" -> repl flambda ()
    | Some l when String.is_prefix l ":t" ->
        begin match c.type_of, String.lsplit2 ~on:' ' l with
        | Some type_of, Some (_, s) -> type_of s |> print_endline
        | None, _ -> print_endline "usage: :type <expr>"
        | _, None -> printf ":type not supported in %s\n" c.name
        end;
        repl c ()
    | Some l ->
        c.compile l
          |> Ulambda.unchurch
          |> Ulambda.pretty
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
  repl tilambda ()
