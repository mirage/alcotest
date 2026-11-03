(* On Windows, the per-test [dup2] that redirects stdout to the test's output
   file closes the HANDLE cached in [Unix.stdout]. Domains that keep opening
   files recycle that HANDLE value, so a later [Unix.create_process] using
   [Unix.stdout] gets an invalid or unrelated handle. *)

let domains = 4
let iterations = 3000
let stop = Atomic.make false

let churn i =
  Domain.spawn (fun () ->
      let path = Printf.sprintf "churn%d.tmp" i in
      while not (Atomic.get stop) do
        Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT ] 0o600 |> Unix.close
      done)

let create_process_with_std_handles () =
  Unix.create_process "cmd.exe"
    [| "cmd.exe"; "/c"; "exit"; "0" |]
    Unix.stdin Unix.stdout Unix.stderr
  |> Unix.waitpid []
  |> ignore

let () =
  let open Alcotest in
  let churners = List.init domains churn in
  Fun.protect
    ~finally:(fun () ->
      Atomic.set stop true;
      List.iter Domain.join churners)
    (fun () ->
      run ~bail:true ~compact:true __FILE__
        [
          ( "create_process",
            List.init iterations (fun i ->
                test_case (string_of_int i) `Quick
                  create_process_with_std_handles) );
        ])
