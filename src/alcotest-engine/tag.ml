open! Import

(** A universal type, into which any other type can be injected and (partially)
    recovered. *)
module Univ : sig
  type t

  val create : unit -> ('a -> t) * (t -> 'a option)
end = struct
  type t = ..

  let create : type a. unit -> (a -> t) * (t -> a option) =
   fun () ->
    let module M = struct
      type t += Case of a option
    end in
    ((fun x -> M.Case (Some x)), function M.Case x -> x | _ -> None)
end

module Key = struct
  type 'a t = {
    type_id : 'a Type_id.t;
    id : int;
    inj : 'a -> Univ.t;
    prj : Univ.t -> 'a option;
    name : string;
    pp_data : Format.formatter -> 'a -> unit;
  }

  let gen_id =
    let counter = ref (-1) in
    fun () ->
      incr counter;
      !counter

  let create ~name ~pp_data =
    let inj, prj = Univ.create () in
    { type_id = Type_id.create (); id = gen_id (); inj; prj; name; pp_data }

  let pp ppf spec = Format.fprintf ppf "(spec %s)" spec.name

  type 'a key = 'a t

  module Boxed = struct
    type t = V : 'a key -> t [@@ocaml.unboxed]

    let compare (V x) (V y) = Int.compare x.id y.id
  end
end

type t = V : 'a Key.t * 'a -> t

let v k v = V (k, v)
let pp ppf (V ({ name; pp_data; _ }, v)) = Fmt.pf ppf "%s = %a" name pp_data v
let const ~name v = V (Key.create ~name ~pp_data:Fmt.(const string name), v)

module Set = struct
  (** A tag set is a map from (boxed) definitions to tags. *)

  module Map = Map.Make (Key.Boxed)

  type nonrec t = t Map.t

  let empty = Map.empty
  let add (V (k, _) as v) s = Map.add (V k) v s

  let find : type a. a Key.t -> t -> a option =
   fun k t ->
    try
      let (V (k', v)) = Map.find (V k) t in
      match Type_id.equal k.type_id k'.type_id with
      | Some Type_id.Refl -> Some v
      | None -> assert false
    with Not_found -> None

  let to_list = Map.bindings >> List.map snd

  let fold_until =
    let rec aux ~f ~finish acc = function
      | [] -> finish acc
      | (_k, v) :: xs -> (
          match f acc v with
          | Continue acc -> aux ~f ~finish acc xs
          | Stop final -> final)
    in
    fun t ~init ~f ~finish -> aux ~f ~finish init (Map.bindings t)
end

module Speed_level = struct
  type t = [ `Quick | `Slow ]

  let tag =
    Key.create ~name:"speed_level"
      ~pp_data:
        (Fmt.of_to_string (function `Quick -> "Quick" | `Slow -> "Slow"))

  let quick = V (tag, `Quick)
  let slow = V (tag, `Slow)
end

module Predicate = struct
  type t = unit -> [ `Run | `Skip ]

  let tag =
    Key.create ~name:"Predicate" ~pp_data:(fun ppf _ ->
        Fmt.pf ppf "Predicate <...>")
end

module Position = struct
  let tag = Key.create ~name:"index" ~pp_data:Fmt.(using snd int)
end
