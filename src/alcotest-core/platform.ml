module type S = sig
  type return

  val time : unit -> float
  val prepare : base:string -> dir:string -> name:string -> unit
  val with_redirect : string -> (unit -> return) -> return
  val setup_std_outputs : ?style_renderer:Fmt.style_renderer -> ?utf_8:bool -> unit -> unit
end

module type MAKER = functor (M : Monad.S) -> S with type return = Pp.run_result M.t

module None (M : Monad.S) = struct
  type return = Pp.run_result M.t

  let time () = 0.
  let prepare ~base:_ ~dir:_ ~name:_ = ()
  let with_redirect _ fn = fn ()
  let setup_std_outputs ?style_renderer:_ ?utf_8:_ () = ()
end
