open Core_kernel.Std
open Sexplib

let p s = s |> Sexp.to_string_hum |> print_endline
let pretty_term t = Tlambda_parsetree.sexp_of_term t |> p
let pretty_ty t = Tlambda_parsetree.sexp_of_ty t |> p

let c s =
  let tl = Tlambda.parse s in
  let _ = Tlambda.typecheck Tlambda.TyCtx.empty tl in
  Tlambda.erase tl
  |> Ulambda.church
  |> Ulambda.reduce Ulambda.predef ~k:(fun x -> x)

let run = fun _ ->
  Errors.run_with_pretty_errors ~err:(fun _ -> exit 1) (fun () ->
    let tcs = [
      "(λx:int.x) 0", `Int 0;
      "(λf:(int->int).f 1) (λx:int.x)", `Int 1;
      "(λx:int.λy:bool.x) 0 #t", `Int 0;
      "(λx:bool.λy:int.y) #t 0", `Int 0;
      "(λx:int.λy:int.x) 0 1", `Int 0;
      "(λx:int.λy:int.y) 0 1", `Int 1;
      "(λx:bool.x) 0", `TypeError;
      "0!", `TypeError;
      "{0}!", `Int 0;
    ] in
    List.iter tcs ~f:(fun (s, exp) ->
      printf "tlambda: %s " s;
      match exp with
      | `Int j ->
          let j' = c s |> Ulambda.unchurch_int in
          printf "->* %d (expected %d)\n" j' j;
          assert (j = j')
      | `TypeError ->
          try begin
            let _ = c s in
            printf "typed unexpectedly\n";
            assert (false);
          end with
          | Tlambda.Tlambda_exception (Tlambda.ForcingNonThunk _)
          | Tlambda.Tlambda_exception (Tlambda.IllTypedApplication _) ->
              printf "type error\n"
    );
  )
