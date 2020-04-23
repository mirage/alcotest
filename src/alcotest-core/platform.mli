module type S = sig
  type return

  val time : unit -> float
  val prepare : base:string -> dir:string -> name:string -> unit
  val with_redirect : string -> (unit -> return) -> return
  val setup_std_outputs : ?style_renderer:Fmt.style_renderer -> ?utf_8:bool -> unit -> unit
end

module type MAKER = functor (M : Monad.S) -> S with type return = Pp.run_result M.t

module None : MAKER
