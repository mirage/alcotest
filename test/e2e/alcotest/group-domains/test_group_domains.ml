(* Groups can be created from several domains at once. A lost group would run as
   a single test called [t], which the selection below does not run. *)
let domains = 4
let per_domain = 1000
let ran = ref 0

let make d =
  List.init per_domain (fun i ->
      let name = Printf.sprintf "g%d_%d" d i in
      Alcotest.group name
        [ Alcotest.test_case name `Quick (fun () -> incr ran) ])

let () =
  let spawned = List.init domains (fun d -> Domain.spawn (fun () -> make d)) in
  let groups = List.concat_map Domain.join spawned in
  Alcotest.run ~and_exit:false ~compact:true ~argv:[| ""; "test"; {|^t\.|} |]
    __FILE__
    [ ("t", groups) ];
  let expected = domains * per_domain in
  if !ran <> expected then
    Printf.ksprintf failwith "ran %d grouped tests instead of %d" !ran expected
