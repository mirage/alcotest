(** This module exists for internal Alcotest use only. It provides no stability
    guarantee. *)

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

type ('a, 'b) continue_or_stop = Continue of 'a | Stop of 'b

module Fun : sig
  val id : 'a -> 'a
  val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
end

module Int : sig
  type t = int

  val compare : t -> t -> int

  module Set : Set.S with type elt = t
end

module String : sig
  include module type of Astring.String

  val length_utf8 : string -> int
  (** Get the length of a string in UTF-8 characters and malformed segments. *)

  val prefix_utf8 : int -> string -> string
  (** [prefix_utf8 n s] is the prefix of [s] containing [n] UTF-8 characters (or
      [s] if it contains fewer than [n] UTF-8 characters). *)
end

module List : sig
  include module type of List

  type 'a t = 'a list

  val rev_take : int -> 'a list -> 'a list
  (** Reverse a list, taking at most the first n elements of the original list. *)

  val concat_map : ('a -> 'b list) -> 'a list -> 'b list
  val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  val lift_result : ('a, 'b) result t -> ('a t, 'b t) result

  val fold_result :
    'a t ->
    init:'acc ->
    f:('acc -> 'a -> ('acc, 'e) result) ->
    ('acc, 'e) result

  val find_duplicate : compare:('a -> 'a -> int) -> 'a t -> 'a option
  val init : int -> (int -> 'a) -> 'a list
end

module Result : sig
  val map : ('a -> 'b) -> ('a, 'e) result -> ('b, 'e) result

  module Syntax : sig
    val ( let+ ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result
    val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
  end
end

module Option : sig
  val map : ('a -> 'b) -> 'a option -> 'b option
  val is_some : _ option -> bool
  val get_exn : 'a option -> 'a
  val value : default:'a -> 'a option -> 'a
  val value_lazy : default:'a Lazy.t -> 'a option -> 'a
  val ( || ) : 'a option -> 'a option -> 'a option
end

module Type_id : sig
  type 'a t
  type (_, _) equal = Refl : ('a, 'a) equal

  val create : unit -> 'a t
  val equal : 'a t -> 'b t -> ('a, 'b) equal option
  val cast : 'a t -> 'b t -> 'a -> 'b option
end

module Source_code_position : sig
  type here = Lexing.position
  type pos = string * int * int * int
end

module Cmdliner_syntax : sig
  open Cmdliner

  val ( let+ ) : 'a Term.t -> ('a -> 'b) -> 'b Term.t
  val ( and+ ) : 'a Term.t -> 'b Term.t -> ('a * 'b) Term.t
  val ( >>| ) : 'a Term.t -> ('a -> 'b) -> 'b Term.t
end
