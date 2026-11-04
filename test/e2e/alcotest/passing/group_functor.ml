(* Suites in the shape of irmin's: a functor applied inside a function called
   once per backend, literal tuples, and a shadowed [Alcotest]. *)

module Alcotest = struct
  include Alcotest

  let quick name f = test_case name `Quick f
end

let ran = ref []

module type Backend = sig
  val name : string
end

module Make (B : Backend) = struct
  let f test () = ran := (B.name ^ ":" ^ test) :: !ran
  let tests = [ ("x", f "graph.x"); ("y", f "graph.y") ]
  let basic = f "basic"
  let last = f "last"
end

let suite speed (module B : Backend) =
  let module T = Make (B) in
  ( B.name,
    [
      ("basic", speed, T.basic);
      Alcotest.group "Graph"
        (List.map (fun (n, test) -> (n, speed, test)) T.tests);
      Alcotest.quick "last" T.last;
    ] )

let backend name =
  (module struct
    let name = name
  end : Backend)

let tests = List.map (suite `Quick) [ backend "mem"; backend "git" ]

(* The same list twice: the same tuples are reported under each name. *)
let shared =
  let module T = Make (struct
    let name = "shared"
  end) in
  let cases = [ Alcotest.group "g" [ Alcotest.quick "z" (T.f "z") ] ] in
  [ ("first", cases); ("second", cases) ]

let run args =
  ran := [];
  Alcotest.run ~and_exit:false ~verbose:true
    ~argv:(Array.of_list ("" :: args))
    "functor" (tests @ shared);
  List.rev !ran

let row (args, expected) =
  Alcotest.test_case
    (String.concat " " ("run" :: args))
    `Quick
    (fun () -> Alcotest.(check (list string)) "ran" expected (run args))

(* First the reported names and their order, then what each selection runs. *)
let () =
  Alcotest.run ~and_exit:false "functor" (tests @ shared);
  let backend b =
    List.map (fun t -> b ^ ":" ^ t) [ "basic"; "graph.x"; "graph.y"; "last" ]
  in
  Alcotest.run __FILE__
    [
      ( "selection",
        List.map row
          [
            ([], backend "mem" @ backend "git" @ [ "shared:z"; "shared:z" ]);
            ([ "test"; {|^mem\.Graph$|} ], [ "mem:graph.x"; "mem:graph.y" ]);
            ([ "test"; {|^git\.Graph$|} ], [ "git:graph.x"; "git:graph.y" ]);
            ([ "test"; "^git$" ], [ "git:basic"; "git:last" ]);
            ([ "test"; {|^second\.g$|} ], [ "shared:z" ]);
          ] );
    ]
