open Core_kernel.Std

type expect = [
  `Int of int
| `Bool of bool
| `Bottom
| `TypeError
| `Thunk
| `AlphaEqv of string
]

module type Output_t = sig
  val before_suite: unit -> unit
  val after_suite: unit -> unit
  val test_case_result: string -> string -> string option -> unit
end

module Stdout(T: sig
  val name: string
end) = struct
  let before_suite () = ()
  let after_suite () = ()
  let test_case_result s v _ = printf "%s: %s ⟶ %s\n" T.name s v
end

module Markdown(T: sig
  val name: string
  val out: Out_channel.t
end) = struct
  open T
  let before_suite () = fprintf out "### Examples for %s\n" name
  let after_suite () = fprintf out "\n"
  let test_case_result s v = function
    | Some note -> fprintf out "`%s` ⟶ `%s` ※ %s\n\n" s v note
    | _ -> fprintf out "`%s` ⟶ `%s`\n\n" s v
end

module Make(T: sig
  type t
  val name: string
  val compile: string -> t
  val int_of_v: t -> int
  val bool_of_v: t -> bool
  val is_thunk: t -> bool
  val cases: (string * expect * string option) list

  val alpha_equivalent: t -> Clambda.value -> bool
end) = struct
  include T
  let run (module O: Output_t) () =
    O.before_suite ();
    List.iter cases ~f:(fun (s, exp, note) ->
    match exp with
    | `Int j ->
        let j' = compile s |> int_of_v in
        assert (j = j');
        O.test_case_result s (string_of_int j') note
    | `Bool b ->
        let b' = compile s |> bool_of_v in
        assert (b = b');
        O.test_case_result s (string_of_bool b') note
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
        | Flambda.Flambda_exception (Flambda.IllTypedTypeApplication)
        | Tilambda.Tilambda_exception Tilambda.Unification_failed ->
            O.test_case_result s "type error" note
        end
    | `Bottom ->
        begin try begin
          let _ = compile s in
          eprintf "did not reach bottom\n";
          assert (false);
        end with
        | Clambda.Clambda_exception Clambda.ReachedBottom ->
            O.test_case_result s "reached bottom" note
        end
    | `Thunk ->
        assert (compile s |> is_thunk);
        O.test_case_result s "{..}" note
    | `AlphaEqv s' ->
        let v = Clambda.parse_value s' in
        assert (alpha_equivalent (compile s) v);
        O.test_case_result s (s' ^ " (α-equiv)") note
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
  val compile: string -> Clambda.value
  val cases: (string * expect * string option) list
end) = Make(struct
  include T
  type t = Clambda.value
  let int_of_v v = Ulambda.(unchurch_int v |> ok_exn)
  let bool_of_v v = Ulambda.(unchurch_bool v |> ok_exn)
  let is_thunk = function
    | `Thunk _ -> true
    | _ -> false
  let alpha_equivalent = Clambda.alpha_equivalent
end)
