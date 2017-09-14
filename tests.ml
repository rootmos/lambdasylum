open Core_kernel.Std

let () =
  Ulambda_tests.run ();
  Tlambda_tests.run ();
  Flambda_tests.run ()

let () = Out_channel.with_file (Sys.getenv "README") ~f:(fun out ->
  let nl () = Out_channel.newline out in
  let o s = Out_channel.output_string out s; nl () in

  o "# Lambdasylum";
  o "[![Build Status](https://travis-ci.org/rootmos/lambdasylum.svg?branch=master)](https://travis-ci.org/rootmos/lambdasylum)";
  nl ();
  o "The lambda asylum is a place to try out different kinds of lambda calculi.";
  o "Currently the following calculi are implemented:";
  o "* [untyped lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) (in [`ulambda`](../master/ulambda.ml))";
  o "  with [Church encoded](https://en.wikipedia.org/wiki/Church_encoding) natural numbers and booleans";
  o "* [simply type lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus) (in [`tlambda`](../master/tlambda.ml))";
  o "* [System F](https://en.wikipedia.org/wiki/System_F) (in [`flambda`](../master/flambda.ml))";
  nl ();

  o "## Usage";
  o "Simplest way to try it out is by using Docker:";
  o "```";
  o "docker run -it rootmos/lambdasylum";
  o "```";
  nl ();

  Ulambda_tests.markdown out;
  Tlambda_tests.markdown out;
  Flambda_tests.markdown out;
)
