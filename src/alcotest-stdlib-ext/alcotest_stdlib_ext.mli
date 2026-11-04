(** This module exists for internal Alcotest use only. It provides no stability
    guarantee. *)

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

module Int : sig
  module Set : Set.S with type elt = int
end

module String : sig
  include module type of Astring.String

  val fold_utf8 : ('a -> int -> Uchar.utf_decode -> 'a) -> 'a -> string -> 'a
  (** [fold_utf8 f acc s] folds [f] over the UTF-8 decodes of [s], with the byte
      position of each. *)

  val length_utf8 : string -> int
  (** Get the length of a string in UTF-8 characters and malformed segments. *)

  val prefix_utf8 : int -> string -> string
  (** [prefix_utf8 n s] is the prefix of [s] containing [n] UTF-8 characters (or
      [s] if it contains fewer than [n] UTF-8 characters). *)
end

module List : sig
  include module type of List

  type 'a t = 'a list

  val rev_head : int -> 'a list -> 'a list
  (** Reverse a list, taking at most the first n elements of the original list.
  *)

  val lift_result : ('a, 'b) result t -> ('a t, 'b t) result
end

module Option : sig
  include module type of Option

  val ( || ) : 'a option -> 'a option -> 'a option
end

module Path : sig
  val ancestors : sep:string -> string -> string list
  (** [ancestors ~sep path] is the directories leading to [path], outermost
      first. *)
end

module Cmdliner_syntax : sig
  open Cmdliner

  val ( let+ ) : 'a Term.t -> ('a -> 'b) -> 'b Term.t
  val ( and+ ) : 'a Term.t -> 'b Term.t -> ('a * 'b) Term.t
  val ( >>| ) : 'a Term.t -> ('a -> 'b) -> 'b Term.t
end
