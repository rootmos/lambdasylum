open Core_kernel.Std

let () = Errors.run_with_pretty_errors ~err:(fun _ -> exit 1) (fun () ->
  Clambda_tests.run ();
  Ulambda_tests.run ();
  Tlambda_tests.run ();
  Flambda_tests.run ();
  Tilambda_tests.run ()
)

let () = Out_channel.with_file (Sys.getenv "README") ~f:(fun out ->
  let nl () = Out_channel.newline out in
  let o s = Out_channel.output_string out s; nl () in

  o "# Lambdasylum";
  o "[![Build Status](https://travis-ci.org/rootmos/lambdasylum.svg?branch=master)](https://travis-ci.org/rootmos/lambdasylum)";
  nl ();
  o "The lambda asylum is a place to try out different kinds of lambda calculi.";
  o "Currently the following calculi are implemented:";
  o "* a core calculus with thunks (in [`clambda`](./clambda.ml)";
  o "* an [untyped lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) (in [`ulambda`](../master/ulambda.ml))";
  o "  with [Church encoded](https://en.wikipedia.org/wiki/Church_encoding) natural numbers and booleans";
  o "* a [simply type lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus) (in [`tlambda`](../master/tlambda.ml))";
  o "* a [System F](https://en.wikipedia.org/wiki/System_F)-style calculs (in [`flambda`](../master/flambda.ml))";
  o "* a [Hindley-Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system)-style type-inferred calculi (in [`tilambda`](./tilambda.ml)";
  nl ();

  o "![calculi](./calculi.png)";
  nl ();

  o "## Usage";
  o "Simplest way to try it out is by using Docker:";
  o "```";
  o "docker run -it rootmos/lambdasylum";
  o "```";
  nl ();

  Clambda_tests.markdown out;
  Ulambda_tests.markdown out;
  Tlambda_tests.markdown out;
  Flambda_tests.markdown out;
)
