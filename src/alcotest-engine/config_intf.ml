module Types = struct
  type bound = [ `Unlimited | `Limit of int ]
  type filter_result = [ `Run | `Skip ]

  type filter_v1 = name:string -> index:int -> filter_result
  and filter_v2 = t -> Tag.Set.t -> filter_result
  and filter = [ `V1 of filter_v1 | `V2 of filter_v2 ]

  and t =
    < and_exit : bool
    ; verbose : bool
    ; compact : bool
    ; tail_errors : bound
    ; quick_only : bool
    ; show_errors : bool
    ; json : bool
    ; filter : filter_v2
    ; log_dir : string
    ; bail : bool
    ; record_backtrace : bool >

  type 'a with_options =
    ?and_exit:bool ->
    ?verbose:bool ->
    ?compact:bool ->
    ?tail_errors:bound ->
    ?quick_only:bool ->
    ?show_errors:bool ->
    ?json:bool ->
    ?filter:filter ->
    ?log_dir:string ->
    ?bail:bool ->
    ?record_backtrace:bool ->
    'a
end

module type Config = sig
  include module type of Types

  module User : sig
    type t
    (** The type of configurations supplied by the user, with defaults not yet
        supplied. *)

    val create : (unit -> t) with_options
    (** Build a config object with the supplied options. *)

    val kcreate : (t -> 'a) -> 'a with_options
    (** Like [create], but passes the constructed config to a continuation
        rather than returning directly. *)

    val term : and_exit:bool -> record_backtrace:bool -> t Cmdliner.Term.t
    (** [term] provides a command-line interface for building configs. *)

    val ( || ) : t -> t -> t
    (** Merge two configs, with fields from the left taking priority over those
        in the right. *)

    (** {2 Accessors} *)

    val quick_only : t -> bool
    val and_exit : t -> bool
    val record_backtrace : t -> bool
  end

  val apply_defaults : default_log_dir:string Lazy.t -> User.t -> t
end
