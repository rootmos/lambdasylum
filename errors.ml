open Printf

let run_with_pretty_errors ?(err=raise) f =
  try f () with
  | Ulambda.Ulambda_exception ue as e ->
      eprintf "ulambda: %s\n" (Ulambda.explain ue);
      err e
