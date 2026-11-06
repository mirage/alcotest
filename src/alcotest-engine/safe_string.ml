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

(* The longest prefix of [s] with at most [n] bytes that does not end in the
   middle of a character. *)
let prefix_bytes n s =
  let rec go i =
    if i > 0 && Char.code s.[i] land 0xC0 = 0x80 then go (i - 1) else i
  in
  if String.length s <= n then s else String.with_range ~len:(go n) s

let max_file = 128
let escape_char c = Fmt.str "U+%04X" (Char.code c)

(* Windows reserves these names for devices, even with an extension. *)
let is_device s =
  let base =
    match String.cut ~sep:"." s with Some (base, _) -> base | None -> s
  in
  match String.Ascii.uppercase base with
  | "CON" | "PRN" | "AUX" | "NUL" -> true
  | b ->
      String.length b = 4
      && (String.is_prefix ~affix:"COM" b || String.is_prefix ~affix:"LPT" b)
      && b.[3] >= '1'
      && b.[3] <= '9'

let file { raw; escaped = s } =
  let n = String.length s in
  let s =
    if n > 0 && (s.[0] = '.' || is_device s) then
      escape_char s.[0] ^ String.with_range ~first:1 s
    else s
  in
  (* Windows drops a trailing dot or space from a file name. *)
  let n = String.length s in
  let s =
    if n > 0 && (s.[n - 1] = '.' || s.[n - 1] = ' ') then
      String.with_range ~len:(n - 1) s ^ escape_char s.[n - 1]
    else s
  in
  if String.length s <= max_file then s
  else
    let digest = String.with_range ~len:8 (Digest.to_hex (Digest.string raw)) in
    prefix_bytes (max_file - 9) s ^ "-" ^ digest

let pp = Fmt.using (fun { raw; _ } -> raw) Fmt.string
let length { raw; _ } = String.length_utf8 raw
let equal t t' = String.equal t.escaped t'.escaped
let compare t t' = String.compare t.escaped t'.escaped
