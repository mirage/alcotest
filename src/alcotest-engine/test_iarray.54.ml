module type S = sig
  type 'a t

  val iarray : 'a t -> 'a Iarray.t t
  (** [iarray t] tests immutable arrays of [t]s. Available from OCaml 5.4. *)
end

module Make (T : sig
  type 'a t

  val map : ('b -> 'a) -> 'a t -> 'b t
  val array : 'a t -> 'a array t
end) =
struct
  let iarray e = T.map Iarray.to_array (T.array e)
end
