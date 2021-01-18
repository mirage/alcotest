type t = Config.filter_v2

val v : (Tag.Set.t -> [ `Run | `Skip ]) -> t

val ( ++ ) : t -> t -> t
(** [f ++ g] is the filter that runs only tests that are run by both [f] {i and}
    [g]. *)

val default : t

(** Internal: *)

val apply : t -> Config.t -> Tag.Set.t -> [ `Run | `Skip ]
val quick_only_config : t
val only_if : t
