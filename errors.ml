open Printf

let run_with_pretty_errors ?(err=raise) f =
  try f () with
  | Ulambda.Ulambda_exception ue as e ->
      eprintf "ulambda: %s\n" (Ulambda.explain ue);
      err e
  | Tlambda.Tlambda_exception te as e ->
      eprintf "tlambda: %s\n" (Tlambda.explain te);
      err e
  | Tilambda.Tilambda_exception te as e ->
      eprintf "tilambda: %s\n" (Tilambda.explain te);
      err e
  | Flambda.Flambda_exception fe as e ->
      eprintf "flambda: %s\n" (Flambda.explain fe);
      err e
  | Clambda.Clambda_exception ce as e ->
      eprintf "clambda: %s\n" (Clambda.explain ce);
      err e
  | Bindings.Bindings_exception (be, ss) as e ->
      eprintf "bindings: %s\n" (Bindings.explain ss be);
      err e
