open Core_kernel.Std

type expect = [`Int of int | `Bool of bool | `Bottom | `TypeError | `Thunk]

module type Output_t = sig
  val before_suite: unit -> unit
  val after_suite: unit -> unit
  val test_case_result: string -> string -> unit
end

module Stdout(T: sig
  val name: string
end) = struct
  let before_suite () = ()
  let after_suite () = ()
  let test_case_result s v = printf "%s: %s ⟶ %s\n" T.name s v
end

module Markdown(T: sig
  val name: string
  val out: Out_channel.t
end) = struct
  open T
  let before_suite () = fprintf out "## Examples for `%s`\n" name
  let after_suite () = fprintf out "\n"
  let test_case_result s v = fprintf out "`%s` ⟶ `%s`\n\n" s v
end

module Make(T: sig
  type t
  val name: string
  val compile: string -> t
  val int_of_v: t -> int
  val bool_of_v: t -> bool
  val is_thunk: t -> bool
  val cases: (string * expect) list
end) = struct
  include T
  let run (module O: Output_t) () =
    O.before_suite ();
    List.iter cases ~f:(fun (s, exp) ->
    match exp with
    | `Int j ->
        let j' = compile s |> int_of_v in
        assert (j = j');
        O.test_case_result s (string_of_int j')
    | `Bool b ->
        let b' = compile s |> bool_of_v in
        assert (b = b');
        O.test_case_result s (string_of_bool b')
    | `TypeError ->
        begin try begin
          let _ = compile s in
          eprintf "typed unexpectedly\n";
          assert (false);
        end with
        | Tlambda.Tlambda_exception (Tlambda.ForcingNonThunk _)
        | Tlambda.Tlambda_exception (Tlambda.IllTypedApplication _)
        | Flambda.Flambda_exception (Flambda.ForcingNonThunk _)
        | Flambda.Flambda_exception (Flambda.IllTypedApplication _)
        | Flambda.Flambda_exception (Flambda.IllTypedTypeApplication) ->
            O.test_case_result s "type error"
        end
    | `Bottom ->
        begin try begin
          let _ = compile s in
          eprintf "did not reach bottom\n";
          assert (false);
        end with
        | Ulambda.Ulambda_exception Ulambda.ReachedBottom ->
            O.test_case_result s "reached bottom"
        end
    | `Thunk ->
        assert (compile s |> is_thunk);
        O.test_case_result s "{..}"
  );
  O.after_suite ()

  let stdout = (module Stdout(struct let name = name end): Output_t)
  let markdown out =
    (module Markdown(struct
      let name = name
      let out = out
    end): Output_t)
end

module Make2(T: sig
  val name: string
  val compile: string -> Ulambda.value
  val cases: (string * expect) list
end) = Make(struct
  include T
  type t = Ulambda.value
  let int_of_v v = Ulambda.Church.(unchurch_int v |> ok_exn)
  let bool_of_v v = Ulambda.Church.(unchurch_bool v |> ok_exn)
  let is_thunk = function
    | `Thunk _ -> true
    | _ -> false
end)
