type index = int

type 'a command =
  | Insert of { expected : index; actual : index }
      (** Insert the element [actual.(actual)] at [expected.(expected)]. *)
  | Delete of { expected : index }
      (** Delete the element at [expected.(expected)]. *)
  | Substitute of { expected : index; actual : index }
      (** Set [expected.(expected)) := actual.(actual)]. *)

type 'a edit_script = 'a command list

type ('a, _) typ =
  | Array : ('a, 'a array) typ
  | List : ('a, 'a list) typ
  | String : (char, string) typ

val levenshtein_script :
  ('a, 'container) typ ->
  equal:('a -> 'a -> bool) ->
  'container ->
  'container ->
  'a edit_script
(** [O(n^2)]-space computation of Levenshtein edit scripts. Guarantees to be
    [O(n)] time in the case that the containers are equal. *)
