type t = Config.filter_v2

let v f _ = f

let ( ++ ) f g a b =
  match (f a b, g a b) with `Run, `Run -> `Run | _, _ -> `Skip

let only_if _ s =
  match Tag.Set.find Tag.Predicate.tag s with Some p -> p () | None -> `Run

let quick_only_config c s =
  match Tag.Set.find Tag.Speed_level.tag s with
  | Some `Slow when c#quick_only -> `Skip
  | _ -> `Run

let default = only_if ++ quick_only_config
let apply f = f
