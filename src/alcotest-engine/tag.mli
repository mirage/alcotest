open! Import

module Key : sig
  type 'a t

  val create : name:string -> pp_data:(Format.formatter -> 'a -> unit) -> 'a t
  val pp : _ t Fmt.t
end

type t

val v : 'a Key.t -> 'a -> t
val pp : t Fmt.t
val const : name:string -> 'a -> t

module Set : sig
  type tag
  type t

  val empty : t
  val add : tag -> t -> t
  val find : 'a Key.t -> t -> 'a option
  val to_list : t -> tag list

  val fold_until :
    t ->
    init:'a ->
    f:('a -> tag -> ('a, 'b) continue_or_stop) ->
    finish:('a -> 'b) ->
    'b
end
with type tag := t

module Speed_level : sig
  type tag
  type t = [ `Quick | `Slow ]

  val tag : t Key.t
  val quick : tag
  val slow : tag
end
with type tag := t

module Position : sig
  val tag : (string * int) Key.t
end

module Predicate : sig
  type t = unit -> [ `Run | `Skip ]

  val tag : t Key.t
end
