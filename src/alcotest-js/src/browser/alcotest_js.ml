open Js_of_ocaml
open Brr

let state = Current_ansi.create ()

let output =
  let out = El.div [ El.h1 [ El.txt' "Alcotest-js" ] ] in
  El.set_at (Jstr.v "style")
    (Some
       (Jstr.v
          "font-family: monospace; color: #e8e8e8; background: #131212; \
           padding: 2em;"))
    out;
  El.(set_prop Prop.id (Jstr.v "output") out);
  let style = El.style [ El.txt' Current_ansi.css ] in
  El.append_children (Document.body G.document) [ style; out ];
  out

let get_or_make name =
  match Document.find_el_by_id G.document (Jstr.v name) with
  | Some v -> v
  | None ->
      let d = El.div [] in
      El.append_children output [ d ];
      El.(set_prop Prop.id (Jstr.v name) d);
      d

let append name s =
  let s = Current_ansi.process state s in
  let p = El.pre [] in
  El.to_jv p |> fun jv ->
  Jv.set jv "innerHTML" (Jv.of_string s);
  El.append_children (get_or_make name) [ p ]

module Js_platform (M : Alcotest_engine.Monad.S) = struct
  module M = Alcotest_engine.Monad.Extend (M)
  open M.Syntax

  let time = Unix.gettimeofday
  let getcwd () = ""
  let stdout_isatty () = true
  let stdout_columns () = Some 0

  let setup_std_outputs ?style_renderer:_ =
    Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty

  (* Pre-4.07 doesn't support empty variant types. *)
  type file_descriptor = string

  let log_trap_supported = false
  let prepare_log_trap ~root:_ ~uuid:_ ~name:_ = ()

  let file_exists name =
    try
      ignore (Sys_js.read_file ~name);
      true
    with Not_found -> false

  let open_write_only name =
    Sys_js.create_file ~name ~content:"";
    name

  let close _ = ()

  let with_redirect name fn =
    let* () = M.return () in
    Fmt.(flush stdout) ();
    Fmt.(flush stderr) ();
    Sys_js.set_channel_flusher stdout (fun content -> append name content);
    Sys_js.set_channel_flusher stderr (fun content -> append name content);
    let+ r = try fn () >|= fun o -> `Ok o with e -> M.return @@ `Error e in
    Fmt.(flush stdout ());
    Fmt.(flush stderr ());
    match r with `Ok x -> x | `Error e -> raise e

  let home_directory () =
    Error (`Msg "Home directory not available for the MirageOS platform")
end

module V1 = struct
  include Alcotest_engine.V1.Test

  module T =
    Alcotest_engine.V1.Cli.Make (Js_platform) (Alcotest_engine.Monad.Identity)

  include T
end

include V1
