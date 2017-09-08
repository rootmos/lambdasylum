open Printf

let run_with_pretty_errors ?(err=raise) f =
  try f () with
  | Ulambda.Ulambda_exception ue as e ->
      eprintf "ulambda: %s\n" (Ulambda.explain ue);
      err e
  | Tlambda.Tlambda_exception ue as e ->
      eprintf "tlambda: %s\n" (Tlambda.explain ue);
      err e
