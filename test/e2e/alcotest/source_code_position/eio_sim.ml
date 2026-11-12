(* Mimics the Eio scheduler, which re-raises the exception of a fiber in the
   caller of [Eio_main.run]. *)
let run f = match f () with v -> v | exception e -> raise e [@@inline never]
