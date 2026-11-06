open Astring
open Model
open! Import
include Log_trap_intf

module Make
    (Promise : Monad.EXTENDED)
    (Platform : Platform.S with type 'a promise := 'a Promise.t) =
struct
  open Promise.Syntax

  type state = { dir : string }
  type t = Inactive | Active of state

  (** Take a string path and collapse a leading [$HOME] path segment to [~]. *)
  let maybe_collapse_home path =
    match Platform.home_directory () with
    | Error _ -> path
    | Ok home -> (
        (* Astring doesn't have [cut_prefix]. *)
        match String.is_prefix ~affix:home path with
        | false -> path
        | true ->
            let tail =
              String.Sub.to_string (String.sub ~start:(String.length home) path)
            in
            "~" ^ tail)

  let inactive = Inactive

  let active ~root ~uuid ~suite_name =
    Active { dir = Platform.prepare_log_trap ~root ~suite:suite_name ~uuid }

  let pp_path = Fmt.using maybe_collapse_home Fmt.string

  let iter_lines fpath ~f =
    In_channel.with_open_text fpath (fun ic ->
        let rec go () =
          match In_channel.input_line ic with
          | Some line ->
              f line;
              go ()
          | None -> ()
        in
        go ())

  (** Show the last lines of a log file. *)
  let pp_tail max_lines fpath ppf =
    match max_lines with
    | `Unlimited -> iter_lines fpath ~f:(Fmt.pf ppf "%s@\n")
    | `Limit limit ->
        let rev_lines = ref [] in
        iter_lines fpath ~f:(fun l -> rev_lines := l :: !rev_lines);
        let selected_lines = List.rev_head limit !rev_lines in
        let omitted_count =
          List.length !rev_lines - List.length selected_lines
        in
        let display_lines =
          if omitted_count = 0 then selected_lines
          else
            Fmt.str "... (omitting %i line%a)" omitted_count Pp.pp_plural
              omitted_count
            :: selected_lines
        in
        ListLabels.iter display_lines ~f:(Fmt.pf ppf "%s@\n")

  let log_dir { dir } = dir
  let output_fpath t tname = Filename.concat (log_dir t) (Test_name.file tname)
  let run_dir = function Inactive -> None | Active t -> Some (log_dir t)

  let active_or_exn = function
    | Active t -> t
    | Inactive -> failwith "internal error: no log location"

  let pp_current_run_dir t ppf =
    let t = active_or_exn t in
    pp_path ppf (log_dir t)

  let pp_log_location t tname ppf =
    let t = active_or_exn t in
    let path = output_fpath t tname in
    pp_path ppf path

  let recover_logs t ~tail tname =
    match t with
    | Inactive -> None
    | Active t -> (
        let fpath = output_fpath t tname in
        match Platform.file_exists fpath with
        | false -> None
        | true -> Some (fun ppf -> pp_tail tail fpath ppf))

  let with_captured_logs t tname f x =
    match t with
    | Inactive -> f x
    | Active t ->
        let fd = Platform.open_write_only (output_fpath t tname) in
        let* () = Promise.return () in
        let+ a = Platform.with_redirect fd (fun () -> f x) in
        Platform.close fd;
        a
end
