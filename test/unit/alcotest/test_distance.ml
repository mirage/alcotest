module Distance = Alcotest_engine.Private.Distance

let pp_command ppf = function
  | Distance.Insert { expected; actual } ->
      Fmt.pf ppf "Insert { expected = %d; actual = %d }" expected actual
  | Delete { expected } -> Fmt.pf ppf "Delete { expected = %d }" expected
  | Substitute { expected; actual } ->
      Fmt.pf ppf "Substitute { expected = %d; actual = %d }" expected actual

let test_edit_script = Alcotest.(list (of_pp pp_command))

module Levenshtein = struct
  (* Basic test case *)
  let lev ?(speed = `Quick) a b msg expected =
    Alcotest.test_case msg speed (fun () ->
        Distance.(levenshtein_script Array) ~equal:Int.equal a b
        |> Alcotest.check test_edit_script msg expected)

  let long_arrays =
    let size = 5_000 in
    let long_array1 = Array.init size Fun.id in
    let long_array2 = Array.init size (( + ) size) in
    let expected =
      List.init size (fun i -> Distance.Substitute { expected = i; actual = i })
    in
    lev ~speed:`Slow long_array1 long_array2
      (Fmt.str "Non-equal arrays with %d elements" size)
      expected

  let suite =
    let open Distance in
    let i expected actual = Insert { expected; actual }
    and d expected = Delete { expected }
    and s expected actual = Substitute { expected; actual } in

    [
      lev [| 1 |] [||] "Single delete" [ d 0 ];
      lev [||] [| 1 |] "Single insert" [ i 0 0 ];
      lev [| 1; 2 |] [||] "Multiple deletes" [ d 0; d 1 ];
      lev [||] [| 1; 2 |] "Multiple inserts" [ i 0 0; i 1 1 ];
      lev [| 1 |] [| 2 |] "Single substitute (1/1)" [ s 0 0 ];
      lev [| 10; 2 |] [| 11; 2 |] "Single substitute (1/2)" [ s 0 0 ];
      lev [| 2; 10 |] [| 2; 11 |] "Single substitute (2/2)" [ s 1 1 ];
      lev [| 1; 0; 3 |] [| 1; 2; 3 |] "Single substitute (2/3)" [ s 1 1 ];
      lev
        [| 1; 2; 3; -4; 5; 6; 7; -8; 9; 10 |]
        [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |]
        "Multiple substitutes" [ s 3 3; s 7 7 ];
      lev [| -1; 3; 4; 5 |] [| 1; 2; 3; 4; 5 |] "Substitute then insert"
        [ s 0 0; i 1 1 ];
      lev [| 1; 0; 3 |] [| 1; 2 |] "Substitute then delete" [ s 1 1; d 2 ];
      long_arrays;
    ]
end

let () = Alcotest.run "test_diff" [ ("levenshtein", Levenshtein.suite) ]
