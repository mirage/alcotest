open Astring

module List = struct
  include List

  type 'a t = 'a list

  let filter_map f l =
    let rec inner acc = function
      | [] -> rev acc
      | x :: xs -> (
          match f x with
          | None -> (inner [@tailcall]) acc xs
          | Some y -> (inner [@tailcall]) (y :: acc) xs )
    in
    inner [] l

  let lift_result l =
    List.fold_right
      (fun a b ->
        match (a, b) with
        | Ok o, Ok acc -> Ok (o :: acc)
        | Ok _, Error e -> Error e
        | Error e, Error acc -> Error (e :: acc)
        | Error e, Ok _ -> Error [ e ])
      l (Ok [])
end

module Result = struct
  let map f = function Ok x -> Ok (f x) | Error e -> Error e
end

module Unix = struct
  include Unix

  let mkdir_p path mode =
    let is_win_drive_letter x =
      String.length x = 2 && x.[1] = ':' && Char.Ascii.is_letter x.[0]
    in
    let sep = Filename.dir_sep in
    let rec mk parent = function
      | [] -> ()
      | name :: names ->
          let path = parent ^ sep ^ name in
          ( try Unix.mkdir path mode
            with Unix.Unix_error (Unix.EEXIST, _, _) ->
              if Sys.is_directory path then () (* the directory exists *)
              else Fmt.strf "mkdir: %s: is a file" path |> failwith );
          mk path names
    in
    match String.cuts ~empty:true ~sep path with
    | "" :: xs -> mk sep xs
    (* check for Windows drive letter *)
    | dl :: xs when is_win_drive_letter dl -> mk dl xs
    | xs -> mk "." xs
end

module Fmt = struct
  [@@@warning "-32"]

  (* Re-implement [flush] for pre-0.8.6 compatibility *)
  let flush ppf _ = Format.pp_print_flush ppf ()

  [@@@warning "+32"]

  include Fmt
end
