val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

module String : sig
  include module type of Astring.String

  val length_utf8 : string -> int
  (** Get the length of a string in UTF-8 characters and malformed segments. *)
end

module List : sig
  include module type of List

  type 'a t = 'a list

  val filter_map : ('a -> 'b option) -> 'a list -> 'b list

  val lift_result : ('a, 'b) result t -> ('a t, 'b t) result

  val init : int -> (int -> 'a) -> 'a list
end

module Result : sig
  val map : ('a -> 'b) -> ('a, 'e) result -> ('b, 'e) result
end
