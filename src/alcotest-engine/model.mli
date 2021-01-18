open! Import

module Index : sig
  type t
  (** A value that uniquely identifies a particular test (or group of tests) in
      a suite (expressed as a path of parents names followed by a name). *)

  val rcons : t -> last:bool -> name:Safe_string.t -> t
  (** [rcons ~last ~name group] is the path to the child named [name] in
      [group]. [last] specifies if the child [name] is the last sibling in
      [group] (when ordered lexicographically). *)

  val last_in_parent : t -> bool
  val parent_path : t -> Safe_string.t list
  val leaf_name : t -> Safe_string.t
end

module Run_result : sig
  type t =
    | Ok
    | Exn of Index.t * string * unit Fmt.t
    | Error of Index.t * unit Fmt.t
    | Skip
    | Todo of string

  val is_failure : t -> bool
  (** [is_failure] holds for test results that are error states. *)

  val has_run : t -> bool
end

module Suite (M : Monad.S) : sig
  type 'a test
  type filter_result = [ `Run | `Skip ]
  type filter = Tag.Set.t -> filter_result

  val test :
    name:string ->
    loc:Source_code_position.pos option ->
    tags:Tag.Set.t ->
    ('a -> unit M.t) ->
    'a test

  val group :
    name:string ->
    loc:Source_code_position.pos option ->
    tags:Tag.Set.t ->
    'a test list ->
    'a test

  type 'a t

  val foldi_until :
    filter:filter ->
    ?group:
      (Index.t -> 'acc -> filter_result -> ('acc, 'final) continue_or_stop M.t) ->
    ?test:
      (Index.t ->
      'acc ->
      [ `Run of 'a -> unit M.t | `Skip ] ->
      ('acc, 'final) continue_or_stop M.t) ->
    init:'acc ->
    finish:('acc -> 'final) ->
    'a t ->
    'final M.t
  (** Depth-first traversal over the suite, skipping nodes according to the
      [filter] predicate defined over node {!Tag}s. *)

  val fold :
    filter:filter ->
    group:('acc -> filter_result -> 'acc) ->
    test:('acc -> [ `Run of 'a -> unit M.t | `Skip ] -> 'acc) ->
    init:'acc ->
    'a t ->
    'acc M.t

  val of_tests :
    name:string option ->
    file:string option ->
    loc:Source_code_position.pos option ->
    'a test list ->
    ( 'a t,
      [ `Duplicate_path of string | `No_identifier | `Empty_name | `Empty_file ]
    )
    result

  val identifier : _ t -> string
  (** An escaped form of the suite name (or the file name, if [name = None]). *)

  val pp_name : _ t -> (Format.formatter -> unit) option
  (** Pretty-print the unescaped suite name, if supplied. *)

  val pp_file : _ t -> (Format.formatter -> unit) option
  (** Pretty-print the source file of the suite, if supplied. *)

  val pp : _ t Fmt.t
end
