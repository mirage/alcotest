module type S = sig
  type return

  val time : unit -> float
  val prepare : base:string -> dir:string -> name:string -> unit
  val with_redirect : string -> (unit -> return) -> return
end

(*module Make (M : Monad.EXTENDED) : S with type return = unit M.t *)
module type MAKER = functor (M : Monad.EXTENDED) -> S with type return = Pp.run_result M.t

module None : MAKER
module Unix : MAKER
