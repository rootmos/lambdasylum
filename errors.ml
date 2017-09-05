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
