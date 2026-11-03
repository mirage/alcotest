let a = Iarray.of_list [ 1; 2; 3 ]
let t = Alcotest.(iarray int)

let test_equal () =
  Alcotest.check t "same elements" a (Iarray.of_list [ 1; 2; 3 ]);
  Alcotest.check Alcotest.bool "other element" false
    (Alcotest.equal t a (Iarray.of_list [ 1; 2; 4 ]));
  Alcotest.check Alcotest.bool "other length" false
    (Alcotest.equal t a (Iarray.of_list [ 1; 2 ]))

let test_nested () =
  let t = Alcotest.(iarray (list string)) in
  let a = Iarray.of_list [ [ "a" ]; [] ] in
  Alcotest.check t "nested" a (Iarray.of_list [ [ "a" ]; [] ])

let test_pp () =
  Alcotest.check Alcotest.string "printed" "[|1; 2; 3|]"
    (Format.asprintf "%a" (Alcotest.pp t) a)

let test_v1 () = Alcotest.V1.(check (iarray int)) "V1" a a

let () =
  Alcotest.run "iarray"
    [
      ( "iarray",
        [
          Alcotest.test_case "equal" `Quick test_equal;
          Alcotest.test_case "nested" `Quick test_nested;
          Alcotest.test_case "pp" `Quick test_pp;
          Alcotest.test_case "V1" `Quick test_v1;
        ] );
    ]
