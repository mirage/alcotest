module List = struct
  include List

  let filter_map f l =
    let rec inner acc = function
      | [] -> rev acc
      | x :: xs -> (
          match f x with
          | None -> (inner [@tailcall]) acc xs
          | Some y -> (inner [@tailcall]) (y :: acc) xs )
    in
    inner [] l

  let lift_result l =
    List.fold_right
      (fun a b ->
        match (a, b) with
        | Ok o, Ok acc -> Ok (o :: acc)
        | Ok _, Error e -> Error e
        | Error e, Error acc -> Error (e :: acc)
        | Error e, Ok _ -> Error [ e ])
      l (Ok [])
end
