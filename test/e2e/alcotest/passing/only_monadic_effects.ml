(** Ensures that {!Alcotest.run} does not print when evaluated at monadic type
    (until the resulting computation is explicitly run). We require that effects
    introduced by tests (whether during evaluation {i or} during running the
    computation) are not partially observable during application of
    {!Alcotest.run}. This is because such behaviour could be relied upon as part
    of the public API, when in fact it is dependent on internal implementation
    details.

    This is tested by building a runner over the [Terminal] monad, which reduces
    to [()] immediately on evaluation and so discovers any effects in the
    evaluation of the bottom-most value in the computation. *)

module Terminal : Alcotest.Monad.S = struct
  type 'a t = unit

  let return _ = ()

  let bind () _ = ()

  let catch f on_error = match f () with x -> x | exception ex -> on_error ex
end

module Runner = Alcotest.Core.Make (Terminal)

let () =
  let (_ : unit Terminal.t) =
    Runner.run ~verbose:true "event_ordering"
      [
        ( "alpha",
          [
            Runner.test_case "check + stdout + stderr" `Quick (fun () ->
                Alcotest.(check unit) "SHOULD NOT BE PRINTED" () ();
                Format.printf "stdout: SHOULD NOT BE PRINTED\n";
                Format.eprintf "stderr: SHOULD NOT BE PRINTED\n";
                assert false);
          ] );
      ]
  in
  Format.pp_print_flush Format.std_formatter ();
  Format.pp_print_flush Format.err_formatter ();
  ()
