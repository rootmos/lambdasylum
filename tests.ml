open Core_kernel.Std

let () =
  Ulambda_tests.run ();
  Tlambda_tests.run ();
  Flambda_tests.run ()

let () = Out_channel.with_file (Sys.getenv "README") ~f:(fun out ->
  fprintf out "# Lambdasylum\n\n";
  Ulambda_tests.markdown out;
  Tlambda_tests.markdown out;
  Flambda_tests.markdown out;
)
