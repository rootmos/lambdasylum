open Core_kernel.Std

type expect = [`Int of int | `Bottom | `TypeError | `Thunk]

module Make(T: sig
  type t
  val name: string
  val compile: string -> t
  val int_of_v: t -> int
  val is_thunk: t -> bool
  val cases: (string * expect) list
end) = struct
  include T
  let run () = List.iter cases ~f:(fun (s, exp) ->
    printf "%s: %s ⟶ " name s;
    match exp with
    | `Int j ->
        let j' = compile s |> int_of_v in
        printf "%d\n" j';
        assert (j = j')
    | `TypeError ->
        begin try begin
          let _ = compile s in
          printf "typed unexpectedly\n";
          assert (false);
        end with
        | Tlambda.Tlambda_exception (Tlambda.ForcingNonThunk _)
        | Tlambda.Tlambda_exception (Tlambda.IllTypedApplication _)
        | Flambda.Flambda_exception (Flambda.ForcingNonThunk _)
        | Flambda.Flambda_exception (Flambda.IllTypedApplication _)
        | Flambda.Flambda_exception (Flambda.IllTypedTypeApplication) ->
            printf "type error\n"
        end
    | `Bottom ->
        begin try begin
          let _ = compile s in
          printf "did not reach bottom\n";
          assert (false);
        end with
        | Ulambda.Ulambda_exception Ulambda.ReachedBottom ->
            printf "reached bottom\n"
        end
    | `Thunk ->
        assert (compile s |> is_thunk);
        printf "{...}\n";
  );
end

module Make2(T: sig
  val name: string
  val compile: string -> Ulambda.value
  val cases: (string * expect) list
end) = Make(struct
  include T
  type t = Ulambda.value
  let int_of_v = Ulambda.unchurch_int
  let is_thunk = function
    | `Thunk _ -> true
    | _ -> false
end)
