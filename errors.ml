open Printf

let run_with_pretty_errors ?(err=raise) f =
  try f () with
  | Ulambda.Ulambda_exception ue as e ->
      eprintf "ulambda: %s\n" (Ulambda.explain ue);
      err e
  | Tlambda.Tlambda_exception te as e ->
      eprintf "tlambda: %s\n" (Tlambda.explain te);
      err e
  | Flambda.Flambda_exception te as e ->
      eprintf "flambda: %s\n" (Flambda.explain te);
      err e
  | Bindings.Bindings_exception (ue, ss) as e ->
      eprintf "bindings: %s\n" (Bindings.explain ss ue);
      err e
