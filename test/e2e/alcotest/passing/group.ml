let ran = ref []
let f name () = ran := name :: !ran

let suite =
  let open Alcotest in
  let case name = test_case name `Quick (f name) in
  let shared = f "shared" in
  [
    ( "top",
      [
        case "t1";
        group "a"
          [
            case "a1";
            group "b" [ case "b1"; test_case "slow" `Slow (f "slow") ];
            case "a2";
          ];
        case "t2";
        test_case "s1" `Quick shared;
        group "a" [ test_case "s2" `Quick shared ];
        ("literal", `Quick, f "literal");
      ] );
    ("other", [ group "c" [ case "c1" ] ]);
    ( "rebuilt",
      [ (fun (n, s, g) -> (n, s, g)) (group "d" [ case "d1"; case "d2" ]) ] );
    ("empty", [ group "e" [] ]);
  ]

(* The inner run is verbose because log traps do not nest: its output would go to
   the log of the outer test. *)
let selected args =
  ran := [];
  Alcotest.run ~and_exit:false ~verbose:true
    ~argv:(Array.of_list ("" :: args))
    "group" suite;
  List.rev !ran

let row (args, expected) =
  Alcotest.test_case
    (String.concat " " ("run" :: args))
    `Quick
    (fun () ->
      Alcotest.(check (list string)) "selected" expected (selected args))

(* First the reported names and their order, then what each selection runs. *)
let () =
  Alcotest.run ~and_exit:false "group" suite;
  Alcotest.run __FILE__
    [
      ( "selection",
        List.map row
          [
            ( [],
              [
                "t1";
                "a1";
                "b1";
                "slow";
                "a2";
                "t2";
                "shared";
                "shared";
                "literal";
                "c1";
                "d1";
                "d2";
              ] );
            ( [ "test"; "-q" ],
              [
                "t1";
                "a1";
                "b1";
                "a2";
                "t2";
                "shared";
                "shared";
                "literal";
                "c1";
                "d1";
                "d2";
              ] );
            ([ "test"; "^top$" ], [ "t1"; "t2"; "shared"; "literal" ]);
            ([ "test"; {|^top\.a$|} ], [ "a1"; "a2"; "shared" ]);
            ([ "test"; {|^top\.a|} ], [ "a1"; "b1"; "slow"; "a2"; "shared" ]);
            ([ "test"; {|^top\.a\.b$|} ], [ "b1"; "slow" ]);
            ([ "test"; {|^other\.c$|} ], [ "c1" ]);
          ] );
    ]
