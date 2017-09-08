open Core_kernel.Std
open Printf

let c s = s
  |> Ulambda.parse
  |> Ulambda.church
  |> Ulambda.reduce Ulambda.predef ~k:(fun x -> x)

let p ul = ul |> Ulambda.pretty |> print_endline
let i = Ulambda.unchurch_int

let () =
  Errors.run_with_pretty_errors ~err:(fun _ -> exit 1) (fun () ->
    let tcs = [
      "1", `Int 1;
      "(λx.x) 1", `Int 1;
      "(\\lambda x.x) 1", `Int 1;

      "1+2", `Int 3;
      "2+1", `Int 3;
      "0+1", `Int 1;
      "1+0", `Int 1;

      "7-2", `Int 5;
      "0-0", `Int 0;
      "1-0", `Int 1;
      "0-1", `Int 0;
      "2-2", `Int 0;

      "3*4", `Int 12;

      "succ 0", `Int 1;
      "succ 1", `Int 2;
      "succ 7", `Int 8;

      "pred 0", `Int 0;
      "pred 1", `Int 0;
      "pred 2", `Int 1;
      "pred 7", `Int 6;

      "if true 1 2", `Int 1;
      "if false 1 2", `Int 2;

      "if (and true true) 1 2", `Int 1;
      "if (and false true) 1 2", `Int 2;
      "if (and true false) 1 2", `Int 2;
      "if (and false false) 1 2", `Int 2;

      "if (or true true) 1 2", `Int 1;
      "if (or false true) 1 2", `Int 1;
      "if (or true false) 1 2", `Int 1;
      "if (or false false) 1 2", `Int 2;

      "if (zero? 0) 1 2", `Int 1;
      "if (zero? 1) 1 2", `Int 2;
      "if (zero? 7) 1 2", `Int 2;

      "if (leq? 3 4) 1 2", `Int 1;
      "if (leq? 3 3) 1 2", `Int 1;
      "if (leq? 4 3) 1 2", `Int 2;

      "if (eq? 3 4) 1 2", `Int 2;
      "if (eq? 3 3) 1 2", `Int 1;
      "if (eq? 4 3) 1 2", `Int 2;

      "⊥", `Bottom;
      "\\bot", `Bottom;

      "{⊥}", `Thunk;
      "{{⊥}}", `Thunk;
      "{0}!", `Int 0;
      "{{0}}!", `Thunk;
      "{{0}}!!", `Int 0;
      "0!", `Int 0;

      "{λx.x} 2", `Int 2;

      "if true 0 {⊥}", `Int 0;
      "if false {⊥} 1", `Int 1;
      "if false {0} ⊥", `Bottom;
      "(if true {0} {⊥})!", `Int 0;
      "(if false {0} {⊥})!", `Bottom;
      "(if true 1 {⊥})!", `Int 1;
      "((if true 1 {2})!)+1", `Int 2;
      "((if false 1 {2})!)+1", `Int 3;

      "(fix (λk.λn.(if (eq? n 1) 1 {(k (n-1))*n})!)) 5", `Int 120;
      "(fix (λk.λn.(if (leq? n 1) 1 {(k (n-1))+(k (n-2))})!)) 7", `Int 21;
    ] in
    List.iter tcs ~f:(fun (s, exp) ->
      printf "reducing: %s " s;
      match exp with
      | `Int j ->
          let j' = c s |> i in
          printf "->* %d (expected %d)\n" j' j;
          assert (j = j')
      | `Thunk ->
          begin match c s with
          | `Thunk _ -> printf "->* {...}\n";
          | _ -> assert (false);
          end
      | `Bottom ->
          try begin
            let _ = c s in
            printf "did not reach bottom\n";
            assert (false);
          end with
          | Ulambda.Ulambda_exception Ulambda.ReachedBottom ->
              printf "reached bottom\n"
    );
  )
