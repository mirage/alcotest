(* These names must stay usable under a local open of [Alcotest]. *)
let quick = 2
let slow = 3
let tag = 4
let ran = ref []
let f name () = ran := name :: !ran

let suite =
  let open Alcotest in
  let tagged name t = test_case name t (f name) in
  let rename (n, s, f) = ("renamed " ^ n, s, f) in
  let wrap (n, s, f) = (n, s, fun x -> f x) in
  [
    ( "tagged",
      [
        tagged "q" `Quick;
        tagged "s" `Slow;
        tagged "d" (`Tag "db");
        tagged "qd" (`And [ `Quick; `Tag "db" ]);
        tagged "sd" Tag.(slow && v "db");
        tagged "oc" Tag.(v "net" || v "crypto");
        tagged "ni" Tag.(not (v "ci"));
        tagged "a" `Always;
      ] );
    ("rebuilt", List.map rename [ tagged "r" (`Tag "r") ]);
    ("wrapped", List.map wrap [ tagged "w" (`Tag "w"); tagged "ws" `Slow ]);
    ("tuples", [ ("t", `Tag "t", f "t") ]);
  ]

(* The inner run is verbose because log traps do not nest: its output goes to
   the log of the outer test. *)
let selected ?quick_only suite args =
  ran := [];
  Alcotest.run ~and_exit:false ~verbose:true ?quick_only
    ~argv:(Array.of_list ("" :: args))
    "tags" suite;
  List.rev !ran

let base = [ "q"; "s"; "ni"; "a"; "ws" ]
let quicks = [ "q"; "ni"; "a" ]

let rows =
  [
    ([], base);
    ([ "-q" ], quicks);
    ([ "--tags=slow" ], base);
    ([ "-q"; "--tags=slow" ], base);
    ([ "--tags=-slow" ], quicks);
    ([ "--tags=-quick" ], [ "s"; "ni"; "a"; "ws" ]);
    ([ "--tags=db" ], [ "q"; "s"; "d"; "qd"; "sd"; "ni"; "a"; "ws" ]);
    ([ "-q"; "--tags=db" ], [ "q"; "d"; "qd"; "ni"; "a" ]);
    ([ "--tags=db,-slow" ], [ "q"; "d"; "qd"; "ni"; "a" ]);
    ([ "--tags=net" ], [ "q"; "s"; "oc"; "ni"; "a"; "ws" ]);
    ([ "--tags=crypto" ], [ "q"; "s"; "oc"; "ni"; "a"; "ws" ]);
    ([ "--tags=ci" ], [ "q"; "s"; "a"; "ws" ]);
    ([ "--tags=net,-quick,-slow" ], [ "oc"; "ni"; "a" ]);
    ([ "--tags=" ], base);
    ([ "--tags=db,,r" ], [ "q"; "s"; "d"; "qd"; "sd"; "ni"; "a"; "r"; "ws" ]);
    ([ "--tags=r" ], [ "q"; "s"; "ni"; "a"; "r"; "ws" ]);
    ([ "--tags=w" ], [ "q"; "s"; "ni"; "a"; "w"; "ws" ]);
    ([ "--tags=t" ], [ "q"; "s"; "ni"; "a"; "ws"; "t" ]);
    ([ "--tags=db,-db" ], base);
    ([ "-e"; "--tags=db" ], [ "q"; "s"; "d"; "qd"; "sd"; "ni"; "a"; "ws" ]);
  ]

let row (args, expected) =
  Alcotest.test_case
    (String.concat " " ("run" :: args))
    `Quick
    (fun () ->
      Alcotest.(check (list string)) "selected" expected (selected suite args))

let test_quick_only () =
  Alcotest.(check (list string))
    "removes slow"
    [ "q"; "d"; "qd"; "ni"; "a" ]
    (selected ~quick_only:true suite [ "--tags=db" ])

let test_shared_function () =
  let count = ref 0 in
  let g () = incr count in
  let suite =
    Alcotest.
      [ ("shared", [ test_case "a" (`Tag "db") g; test_case "b" `Quick g ]) ]
  in
  let (_ : string list) = selected suite [ "--tags=db,-quick" ] in
  Alcotest.(check int) "only the tagged test" 1 !count

let test_always () =
  let suite =
    [
      ( "always",
        [ ("a", `Always, f "a"); ("q", `Quick, f "q"); ("d", `Tag "db", f "d") ]
      );
    ]
  in
  let check name expected args =
    Alcotest.(check (list string)) name expected (selected suite args)
  in
  check "no option" [ "a"; "q" ] [];
  check "quick" [ "a"; "q" ] [ "-q" ];
  check "other tag" [ "a"; "q"; "d" ] [ "--tags=db" ];
  check "removed tags" [ "a" ] [ "--tags=-quick,-slow" ]

let test_group () =
  let open Alcotest in
  let tagged name t = test_case name t (f name) in
  let suite =
    [
      ( "top",
        [
          group "g"
            [
              tagged "q" `Quick;
              tagged "s" `Slow;
              tagged "d" (`Tag "db");
              group "h" [ tagged "hd" (`Tag "db"); tagged "hq" `Quick ];
            ];
        ] );
    ]
  in
  let check name expected args =
    Alcotest.(check (list string)) name expected (selected suite args)
  in
  check "no option" [ "q"; "s"; "hq" ] [];
  check "quick" [ "q"; "hq" ] [ "-q" ];
  check "tag" [ "q"; "s"; "d"; "hd"; "hq" ] [ "--tags=db" ];
  check "only the tag" [ "d"; "hd" ] [ "--tags=db,-quick,-slow" ]

let test_no_shadowing () = Alcotest.(check int "values" 9 (quick + slow + tag))

(* Existing ways to call [test_case], which must compile unchanged. *)
let test_call_forms () =
  let open Alcotest in
  let g () = () in
  let partial = test_case "a" `Quick in
  let restated : string -> speed_level -> (unit -> unit) -> unit test_case =
    test_case
  in
  let mapped =
    List.map (fun (n, s) -> test_case n s g) [ ("b", `Quick); ("c", `Slow) ]
  in
  let suite =
    [ ("forms", [ partial g; restated "d" `Slow g; ("e", `Quick, g) ]) ]
  in
  let (_ : string list) = selected (("mapped", mapped) :: suite) [] in
  ()

let () =
  let open Alcotest in
  run __FILE__
    [
      ("selection", List.map row rows);
      ( "others",
        [
          test_case "quick_only" `Quick test_quick_only;
          test_case "shared function" `Quick test_shared_function;
          test_case "always" `Quick test_always;
          test_case "group" `Quick test_group;
          test_case "no shadowing" `Quick test_no_shadowing;
          test_case "call forms" `Quick test_call_forms;
        ] );
    ]
