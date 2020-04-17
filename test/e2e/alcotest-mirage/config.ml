open Mirage

let main =
  foreign
    ~packages:[
      package "alcotest-mirage";
    ]
    "Unikernel" (job)

let () =
  register "alcotest" [main]
