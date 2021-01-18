type t
(** A UTF-8 encoded string that has been made safe for use in filesystems by
    escaping any "unsafe" characters as their [U+XXXX] notational form. *)

val v : string -> t

val to_string : t -> string
(** Get the escaped form of the given {!Safe_string}. *)

val pp : t Fmt.t
(** Pretty-print the unescaped string. *)

val length : t -> int
(** The approximate number of terminal columns consumed by [pp_name]. *)

val prefix : int -> t -> t
(** Truncate a string to a given number of terminal columns. *)

val equal : t -> t -> bool
val compare : t -> t -> int
