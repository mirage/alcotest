type index = int

type 'a command =
  | Insert of { expected : index; actual : index }
      (** Insert the element [actual.(actual)] at [expected.(expected)]. *)
  | Delete of { expected : index }
      (** Delete the element at [expected.(expected)]. *)
  | Substitute of { expected : index; actual : index }
      (** Set [expected.(expected)) := actual.(actual)]. *)

type 'a edit_script = 'a command list

val levenshtein_script :
  equal:('a -> 'a -> bool) -> 'a list -> 'a list -> 'a edit_script
(** [O(n^2)]-space computation of Levenshtein edit scripts. *)
