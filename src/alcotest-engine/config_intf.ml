module Types = struct
  type bound = [ `Unlimited | `Limit of int ]

  type t =
    < and_exit : bool
    ; verbose : bool
    ; compact : bool
    ; tail_errors : bound
    ; quick_only : bool
    ; show_errors : bool
    ; json : bool
    ; filter : Re.re option * int list option
    ; log_dir : string
    ; bail : bool >

  type 'a with_options =
    ?and_exit:bool ->
    ?verbose:bool ->
    ?compact:bool ->
    ?tail_errors:bound ->
    ?quick_only:bool ->
    ?show_errors:bool ->
    ?json:bool ->
    ?filter:Re.re option * int list option ->
    ?log_dir:string ->
    ?bail:bool ->
    'a
end

module type Config = sig
  include module type of Types

  module User : sig
    type t

    val kcreate : (t -> 'a) -> 'a with_options
    val create : (unit -> t) with_options
    val term : and_exit:bool -> t Cmdliner.Term.t
    val ( || ) : t -> t -> t
    val and_exit : t -> bool
  end

  val apply_defaults : default_log_dir:string -> User.t -> t
end
