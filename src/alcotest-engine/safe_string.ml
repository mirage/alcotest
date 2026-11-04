open! Import

let escape str =
  let add_codepoint buf uchar =
    Uchar.to_int uchar |> Fmt.str "U+%04X" |> Buffer.add_string buf
  in
  let buf = Buffer.create (String.length str * 2) in
  let get_normalized_char () _ d =
    let u = Uchar.utf_decode_uchar d in
    if not (Uchar.utf_decode_is_valid d) then Buffer.add_utf_8_uchar buf u
    else if Uchar.is_char u then
      match Uchar.to_char u with
      | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' | ' ' | '.') as c ->
          Buffer.add_char buf c
      | '/' | '\\' -> Buffer.add_char buf '-'
      | _ -> add_codepoint buf u
    else add_codepoint buf u
  in
  String.fold_utf8 get_normalized_char () str;
  Buffer.contents buf

type t = { raw : string; escaped : string }

let v raw = { raw; escaped = escape raw }
let to_string { escaped; _ } = escaped
let to_unescaped_string { raw; _ } = raw
let pp = Fmt.using (fun { raw; _ } -> raw) Fmt.string
let length { raw; _ } = String.length_utf8 raw
let equal t t' = String.equal t.escaped t'.escaped
let compare t t' = String.compare t.escaped t'.escaped
