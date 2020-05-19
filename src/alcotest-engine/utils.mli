module List : sig
  include module type of List

  type 'a t = 'a list

  val filter_map : ('a -> 'b option) -> 'a list -> 'b list

  val lift_result : ('a, 'b) result t -> ('a t, 'b t) result
end

module Result : sig
  val map : ('a -> 'b) -> ('a, 'e) result -> ('b, 'e) result
end

module Fmt : sig
  include module type of Fmt

  val flush : 'a t
end
