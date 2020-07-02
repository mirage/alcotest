module type S = sig
  val time : unit -> float

  val getcwd : unit -> string

  val prepare : base:string -> dir:string -> name:string -> unit

  type 'a promise

  val with_redirect : string -> (unit -> 'a promise) -> 'a promise

  val setup_std_outputs :
    ?style_renderer:Fmt.style_renderer -> ?utf_8:bool -> unit -> unit

  val home_directory : unit -> (string, [ `Msg of string ]) result
end

module type MAKER = functor (M : Monad.S) -> S with type 'a promise := 'a M.t
