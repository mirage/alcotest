let ( >> ) f g x = x |> f |> g

module Int = struct
  module Set = Set.Make (Int)
end

module String = struct
  include Astring.String

  let fold_utf8 f acc s =
    let rec go acc i =
      if i >= String.length s then acc
      else
        let d = String.get_utf_8_uchar s i in
        go (f acc i d) (i + Uchar.utf_decode_length d)
    in
    go acc 0

  (* Use [Format.utf_8_scalar_width] once OCaml 5.4 is the minimum version. *)
  let length_utf8 = fold_utf8 (fun count _ _ -> count + 1) 0

  let prefix_utf8 uchars string =
    let exception Found_new_length of int in
    try
      let (_ : int) =
        fold_utf8
          (fun count start_pos _ ->
            if count = uchars then raise (Found_new_length start_pos)
            else count + 1)
          0 string
      in
      string
    with Found_new_length l -> String.sub string 0 l
end

module List = struct
  include List

  type 'a t = 'a list

  let rev_head n l =
    let rec aux acc n l =
      match l with
      | x :: xs ->
          if n > 0 then (aux [@tailcall]) (x :: acc) (n - 1) xs else acc
      | [] -> acc
    in
    aux [] n l

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

module Option = struct
  include Option

  let ( || ) a b =
    match (a, b) with
    | None, None -> None
    | (Some _ as x), _ | None, (Some _ as x) -> x
end

module Path = struct
  let ancestors ~sep path =
    let is_win_drive_letter x =
      String.length x = 2 && x.[1] = ':' && Astring.Char.Ascii.is_letter x.[0]
    in
    let rec mk parent = function
      | [] -> []
      | name :: names ->
          let path = parent ^ sep ^ name in
          path :: mk path names
    in
    match String.cuts ~empty:true ~sep path with
    | "" :: xs -> mk "" xs
    (* check for Windows drive letter *)
    | dl :: xs when is_win_drive_letter dl -> mk dl xs
    | xs -> mk "." xs
end

module Cmdliner_syntax = struct
  open Cmdliner

  let ( let+ ) t f = Term.(const f $ t)
  let ( and+ ) a b = Term.(const (fun x y -> (x, y)) $ a $ b)
  let ( >>| ) x f = Term.(app (const f) x)
end
