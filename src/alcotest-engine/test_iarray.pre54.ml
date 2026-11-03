(* The argument matches that of test_iarray.54.ml and nothing uses it here. *)
[@@@ocaml.warning "-32-34"]

module type S = sig
  type 'a t
end

module Make (_ : sig
  type 'a t

  val map : ('b -> 'a) -> 'a t -> 'b t
  val array : 'a t -> 'a array t
end) =
struct end
