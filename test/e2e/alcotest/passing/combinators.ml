let ran = ref []
let f name () = ran := name :: !ran

let suites =
  let open Alcotest in
  let t ?tags name = test ?tags name (f name) in
  [
    suite "plain"
      [
        t "always";
        t ~tags:`Quick "quick";
        t ~tags:`Slow "slow";
        t ~tags:(`Tag "db") "db";
      ];
    suite "groups"
      [
        group "untagged" [ t "u"; t ~tags:`Slow "us" ];
        group ~tags:`Slow "slow"
          [ t "s"; t ~tags:`Quick "sq"; group "deep" [ t "sd" ] ];
        group ~tags:(`Tag "db") "db"
          [ t "d"; group ~tags:(`Tag "net") "net" [ t "dn" ] ];
      ];
    ("tuple", [ test_case "old" `Quick (f "old") ]);
  ]

(* The inner run is verbose because log traps do not nest: its output would go to
   the log of the outer test. *)
let selected args =
  ran := [];
  Alcotest.run ~and_exit:false ~verbose:true
    ~argv:(Array.of_list ("" :: args))
    "combinators" suites;
  List.rev !ran

let row (args, expected) =
  Alcotest.test
    (String.concat " " ("run" :: args))
    (fun () ->
      Alcotest.(check (list string)) "selected" expected (selected args))

let base = [ "always"; "quick"; "slow"; "u"; "us"; "s"; "sq"; "sd"; "old" ]

(* First the reported names and their order, then what each option runs. *)
let () =
  Alcotest.run ~and_exit:false "combinators" suites;
  Alcotest.run __FILE__
    [
      Alcotest.suite "selection"
        (List.map row
           [
             ([], base);
             ([ "-q" ], [ "always"; "quick"; "u"; "old" ]);
             ( [ "--tags=db" ],
               [
                 "always";
                 "quick";
                 "slow";
                 "db";
                 "u";
                 "us";
                 "s";
                 "sq";
                 "sd";
                 "d";
                 "old";
               ] );
             ( [ "--tags=db,net,-quick,-slow" ],
               [ "always"; "db"; "u"; "d"; "dn" ] );
             ([ "test"; {|^groups\.slow\.deep$|} ], [ "sd" ]);
           ]);
    ]
