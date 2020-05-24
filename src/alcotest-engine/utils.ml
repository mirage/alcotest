let ( >> ) f g x = x |> f |> g

module type FUNCTOR = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type FUNCTOR2 = sig
  type ('a, 'e) t

  val map : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
end

module Pair = struct
  type ('a, 'b) t = 'a * 'b

  let map f (a, b) = (f a, f b)
end

module Triple = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  let fst (a, _, _) = a

  let snd (_, b, _) = b

  let trd (_, _, c) = c

  let map f (a, b, c) = (f a, f b, f c)

  let minimum_on ~compare f (a, b, c) =
    let ( <= ) x y = compare x y <= 0 in
    let fa = f a and fb = f b and fc = f c in
    match (fa <= fb, fa <= fc, fb <= fc) with
    | true, true, _ -> a
    | false, _, true -> b
    | _, false, false -> c
    | false, true, false ->
        (* non-transitive : fb < fa <= fc < fb *) assert false
    | true, false, true ->
        (* non-transitive : fa <= fb <= fc > fa *) assert false

  let minimum ~compare = minimum_on ~compare Fun.id
end

module List = struct
  include List

  let rec equal elt_equal xs ys =
    match (xs, ys) with
    | [], [] -> true
    | [], _ :: _ | _ :: _, [] -> false
    | x :: xs, y :: ys -> elt_equal x y && equal elt_equal xs ys

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

  let init n f =
    let rec aux acc i = if i >= n then rev acc else aux (f i :: acc) (i + 1) in
    aux [] 0
end

module Array = struct
  include Array

  let equal elt_equal xs ys =
    let len_x = Array.length xs and len_y = Array.length ys in
    let rec inner i = i = len_x || (elt_equal xs.(i) ys.(i) && inner (i + 1)) in
    len_x = len_y && inner 0
end

module Result = struct
  let map f = function Ok x -> Ok (f x) | Error e -> Error e
end
