type t
(** A UTF-8 encoded string that has been made safe for use in filesystems by
    escaping any "unsafe" characters as their [U+XXXX] notational form. *)

val v : string -> t

val to_string : t -> string
(** Get the escaped form of the given {!Safe_string}. *)

val to_unescaped_string : t -> string
(** Get the raw, unescaped string initially passed to [v]. *)

val file : t -> string
(** [file t] is the escaped form of [t] as a file name on any system. A leading
    dot is escaped, so that it is never [.] or [..] nor hidden, and so are a
    trailing dot or space and the first character of a Windows device name such
    as [NUL]. A name longer than 128 bytes is cut and ends with a digest of [t],
    so that cut names stay distinct. *)

val pp : t Fmt.t
(** Pretty-print the unescaped string. *)

val length : t -> int
(** The approximate number of terminal columns consumed by [pp]. *)

val equal : t -> t -> bool
val compare : t -> t -> int
