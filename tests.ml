open Core_kernel.Std

let () =
  Ulambda_tests.run ();
  Tlambda_tests.run ();
  Flambda_tests.run ()

let () = Out_channel.with_file (Sys.getenv "README") ~f:(fun out ->
  let nl () = Out_channel.newline out in
  let o s = Out_channel.output_string out s; nl () in

  o "# Lambdasylum";
  o "The lambda asylum is a place to try out different kinds of lambda calculi.";
  o "Currently the following calculi are implemented:";
  o "* untyped lambda calculus (in `ulambda`)";
  o "* simply type lambda calculus (in `tlambda`)";
  o "* System F (in `flambda`)";
  nl ();

  Ulambda_tests.markdown out;
  Tlambda_tests.markdown out;
  Flambda_tests.markdown out;
)
