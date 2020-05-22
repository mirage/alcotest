open Utils

type index = int

type 'a command =
  | Insert of { expected : index; actual : index }
      (** Insert the element [actual.(actual)] at [expected.(expected)]. *)
  | Delete of { expected : index }
      (** Delete the element at [expected.(expected)]. *)
  | Substitute of { expected : index; actual : index }
      (** Set [expected.(expected)) := actual.(actual)]. *)

type 'a edit_script = 'a command list

(** Memoize an open recursive function on its first argument. *)
let memoize (type a b) (f : self:(a -> b) -> a -> b) : a -> b =
  let tbl = Hashtbl.create 0 in
  let rec f_memo a =
    match Hashtbl.find_opt tbl a with
    | Some s -> s
    | None ->
        let b = f ~self:f_memo a in
        Hashtbl.add tbl a b;
        b
  in
  f_memo

(** O(n^2) space computation of Levenshtein edit scripts via memoisation. *)
let levenshtein_script (type a) ~(equal : a -> a -> bool) (a : a list)
    (b : a list) : a edit_script =
  (* Track the number of elements dropped from each list in [(i, j)]. Memoize on
     that argument with {!memoize}. *)
  let inner ~self (i, j) = function
    | [], ys ->
        (* We return a [(List.length script, script)] pair to avoid repeatedly
           recalculating the length of the list on each recursive call. *)
        let n = List.length ys in
        ( n,
          List.init n (fun ind ->
              Insert { expected = i + ind; actual = j + ind }) )
    | xs, [] ->
        let n = List.length xs in
        (n, List.init n (fun elt_i -> Delete { expected = i + elt_i }))
    | x :: xs, y :: ys -> (
        match equal x y with
        | true -> self (i + 1, j + 1) (xs, ys)
        | false ->
            let add command (cost, script) = (cost + 1, command :: script) in

            (* Try all three commands and minimise the size of the edit script. *)
            let substitute =
              self (i + 1, j + 1) (xs, ys)
              |> add (Substitute { expected = i; actual = j })
            and delete =
              self (i + 1, j) (xs, y :: ys) |> add (Delete { expected = i })
            and insert =
              self (i, j + 1) (x :: xs, ys)
              |> add (Insert { expected = i; actual = j })
            in
            Triple.minimum_on ~compare:Int.compare fst
              (substitute, delete, insert) )
  in
  (memoize inner) (0, 0) (a, b) |> snd
