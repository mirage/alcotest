val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

module type FUNCTOR = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type FUNCTOR2 = sig
  type ('a, 'e) t

  val map : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
end

module Pair : sig
  type ('a, 'b) t = 'a * 'b

  include FUNCTOR with type 'a t := ('a, 'a) t
end

module Triple : sig
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  include FUNCTOR with type 'a t := ('a, 'a, 'a) t

  val minimum_on :
    compare:('b -> 'b -> int) -> ('a -> 'b) -> ('a, 'a, 'a) t -> 'a
  (** Smallest component of a triple as given by a function [f]. *)
end

module List : sig
  include module type of List

  [@@@warning "-32"]

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val filter_map : ('a -> 'b option) -> 'a t -> 'b t

  val lift_result : ('a, 'b) result t -> ('a t, 'b t) result

  val init : int -> (int -> 'a) -> 'a list

  [@@@warning "+32"]
end

module Array : sig
  include module type of Array

  [@@@warning "-32"]

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  [@@@warning "+32"]
end

module Result : sig
  include FUNCTOR2 with type ('a, 'e) t := ('a, 'e) result
end
