module List : sig
  include module type of List

  val filter_map : ('a -> 'b option) -> 'a list -> 'b list

  val lift_result : ('a, 'b) result t -> ('a t, 'b t) result
end
