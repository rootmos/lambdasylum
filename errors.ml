open Core_kernel.Std
open Printf

let run_with_pretty_errors ?(err=raise) f =
  try f () with
  | Parse_utils.Parse_utils_exception Parse_utils.Parsing as e ->
      eprintf "parsing error\n";
      err e
  | Parse_utils.Parse_utils_exception (Parse_utils.Lexing msg) as e ->
      eprintf "lexing error: %s\n" msg;
      err e
  | Ulambda.Ulambda_exception (Ulambda.Binding n) as e ->
      eprintf "ulambda: ill-scoped identifier: %s\n" n;
      err e
  | Ulambda.Ulambda_exception (Ulambda.ApplicationError) as e ->
      eprintf "ulambda: trying to apply non-lambda\n";
      err e
  | Ulambda.Ulambda_exception (Ulambda.UnchurchError s) as e ->
      eprintf "ulambda: unchurch error: %s\n" s;
      err e
  | Ulambda.Ulambda_exception (Ulambda.ReachedBottom) as e ->
      eprintf "ulambda: code raised runtime error\n";
      err e
