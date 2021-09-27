open Js_of_ocaml

(* Brr-style FFI *)
module Jv = struct
  type t = Js.Unsafe.top Js.t

  let get (v : t) (s : string) : t = Js.Unsafe.get (Js.Unsafe.coerce v) s

  let set (elt : t) (s : string) (v : t) =
    Js.Unsafe.set (Js.Unsafe.coerce elt) s v

  let call (elt : t) (s : string) (args : t array) =
    Js.Unsafe.meth_call elt s args

  let find (elt : t) (s : string) : t option =
    let v = Js.Unsafe.get (Js.Unsafe.coerce elt) s in
    if v == Js.undefined || v == Js.null then None else Some v

  let to_float (v : t) = Js.float_of_number (Js.Unsafe.coerce v)
  let to_int (v : t) = to_float v |> int_of_float
  let of_int i : t = Js.number_of_float (float_of_int i) |> Js.Unsafe.coerce
  let to_bool (v : t) = Js.(to_bool (Unsafe.coerce v))
  let to_string (v : t) = Js.(to_string (Unsafe.coerce v))
  let of_string s : t = Js.string s |> Js.Unsafe.coerce
  let obj = Js.Unsafe.obj
  let error_message t = Js.string_of_error (Js.Unsafe.coerce t)
end

open Astring

exception Node_error of string

let node_to_unix_error err =
  let err = Js.Unsafe.coerce err in
  let code v = Jv.get v "code" |> Jv.to_string in
  match code err with
  | "ENOENT" -> Unix.Unix_error (Unix.ENOENT, "", "Reraised from node")
  | "EINTR" -> Unix.Unix_error (Unix.EINTR, "", "Reraised from node")
  | _ -> Node_error (Js.string_of_error err)

external pure_js_expr : string -> 'a = "caml_pure_js_expr"

let or_error f =
  match f () with
  | exception Js.Error e -> raise (node_to_unix_error e)
  | v -> v

let raise_or_ignore f = or_error f |> ignore

module Tty = struct
  let tty : Jv.t = pure_js_expr "require('tty')"

  (* Hmmmmm.... *)
  let isatty fd =
    or_error @@ fun () -> Jv.call tty "isatty" [| Jv.of_int fd |] |> Jv.to_bool
end

module Process = struct
  let process : Jv.t = pure_js_expr "require('process')"

  let set_stdout_write v =
    let stdout = Jv.get process "stdout" in
    let old_write = Jv.get stdout "write" in
    (Jv.set stdout "write" v, fun () -> Jv.set stdout "write" old_write)

  let stdout_columns () =
    let stdout = Jv.get process "stdout" in
    let columns = Jv.get stdout "columns" in
    Jv.to_int columns

  let set_stderr_write v =
    let stderr = Jv.get process "stderr" in
    let old_write = Jv.get stderr "write" in
    (Jv.set stderr "write" v, fun () -> Jv.set stderr "write" old_write)

  let home () =
    let ( >>= ) f v = Option.bind f v in
    Jv.find process "env" >>= fun env ->
    Jv.find env "HOME" >>= fun home -> Some (Jv.to_string home)

  let cwd () = Jv.call process "cwd" [||] |> Jv.to_string

  let stdout_isatty () =
    let stdout = Jv.get process "stdout" in
    let fd = Jv.get stdout "fd" |> Jv.to_int in
    Tty.isatty fd
end

module Fs = struct
  let fs : Jv.t = pure_js_expr "require('fs')"

  let is_directory path =
    let stat = Jv.call fs "statSync" [| Jv.of_string path |] in
    Jv.call stat "isDirectory" [||] |> Jv.to_bool

  let mkdir s mode =
    raise_or_ignore @@ fun () ->
    Jv.call fs "mkdirSync" [| Jv.of_string s; Jv.of_int mode |]

  let unlink s =
    raise_or_ignore @@ fun () -> Jv.call fs "unlinkSync" [| Jv.of_string s |]

  let symlink ?to_dir v1 v2 =
    let args =
      match to_dir with
      | Some true -> [| Jv.of_string v1; Jv.of_string v2; Jv.of_string "dir" |]
      | Some false ->
          [| Jv.of_string v1; Jv.of_string v2; Jv.of_string "file" |]
      | None -> [| Jv.of_string v1; Jv.of_string v2 |]
    in
    raise_or_ignore @@ fun () -> Jv.call fs "symlinkSync" args |> ignore

  (* TODO: Convert Unix open flags to int *)
  let openfile_for_writing path mode =
    let fd =
      or_error @@ fun () ->
      Jv.call fs "openSync"
        [| Jv.of_string path; Jv.of_string "w"; Jv.of_int mode |]
    in
    Jv.to_int fd

  let close fd =
    raise_or_ignore @@ fun () -> Jv.call fs "closeSync" [| Jv.of_int fd |]

  let exists path =
    try
      (raise_or_ignore @@ fun () -> Jv.call fs "access" [| Jv.of_string path |]);
      true
    with _ -> false

  type write_stream = Jv.t

  let create_write_stream fd flags : write_stream =
    let options =
      Jv.obj [| ("fd", Jv.of_int fd); ("flags", Jv.of_string flags) |]
    in
    Jv.call fs "createWriteStream" [| Jv.of_string "ignored path"; options |]

  let bind_write (jv : write_stream) =
    let write = Jv.get jv "write" in
    Jv.call write "bind" [| jv |]
end

let mkdir_p path mode =
  let is_win_drive_letter x =
    String.length x = 2 && x.[1] = ':' && Char.Ascii.is_letter x.[0]
  in
  let sep = Filename.dir_sep in
  let rec mk parent = function
    | [] -> ()
    | name :: names ->
        let path = parent ^ sep ^ name in
        (try Fs.mkdir path mode
         with Node_error _ ->
           if Fs.is_directory path then () (* the directory exists *)
           else Fmt.strf "mkdir: %s: is a file" path |> failwith);
        mk path names
  in
  match String.cuts ~empty:true ~sep path with
  | "" :: xs -> mk sep xs
  (* check for Windows drive letter *)
  | dl :: xs when is_win_drive_letter dl -> mk dl xs
  | xs -> mk "." xs
