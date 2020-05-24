open Utils

type index = int

type 'a command =
  | Insert of { expected : index; actual : index }
      (** Insert the element [actual.(actual)] at [expected.(expected)]. *)
  | Delete of { expected : index }
      (** Delete the element at [expected.(expected)]. *)
  | Substitute of { expected : index; actual : index }
      (** Set [expected.(expected)) := actual.(actual)]. *)

let map_expected f = function
  | Insert i -> Insert { i with expected = f i.expected }
  | Delete d -> Delete { expected = f d.expected }
  | Substitute s -> Substitute { s with expected = f s.expected }

let map_actual f = function
  | Insert i -> Insert { i with actual = f i.actual }
  | Substitute s -> Substitute { s with actual = f s.actual }
  | Delete _ as d -> d

let insert expected actual = Insert { expected; actual }

let delete expected = Delete { expected }

let substitute expected actual = Substitute { expected; actual }

type 'a edit_script = 'a command list

type 'a subarray = { array : 'a array; origin : int; length : int }

let get { array; origin; _ } i = array.(i + origin)

(** Standard dynamic programming algorithm for Levenshtein edit scripts. This
    works in two phases:

    1. construct a {i cost matrix} of edit distances for each _prefix_ of the
    two strings;

    2. reconstruct an edit script from the cost matrix.

    The standard algorithm uses a cost matrix of size [n * m]. If we only care
    about edit scripts up to some maximum length [b], the space and time
    complexity can be reduced to [O(max (n, m) * b)] (assuming an [O(1)]
    equality function). *)

(** Phase 1: compute the cost matrix, bottom-up. *)
let construct_grid (type a) ~(equal : a -> a -> bool) (a : a subarray)
    (b : a subarray) : int array array =
  let grid_x_length = a.length + 1 and grid_y_length = b.length + 1 in
  let grid = Array.make_matrix grid_x_length grid_y_length 0 in
  let get_grid (i, j) = grid.(i).(j) in

  for i = 0 to a.length do
    for j = 0 to b.length do
      let cost =
        if min i j = 0 then max i j
        else if equal (get a (i - 1)) (get b (j - 1)) then
          get_grid (i - 1, j - 1)
        else
          ((i - 1, j), (i, j - 1), (i - 1, j - 1))
          |> Triple.map get_grid
          |> Triple.minimum ~compare:Int.compare
          |> ( + ) 1
      in

      grid.(i).(j) <- cost
    done
  done;

  grid

(** Phase 2: reconstruct the edit script from the cost matrix. *)
let reconstruct_edit_script grid =
  let rec aux acc = function
    | 0, 0 -> acc
    | i, 0 -> List.init i delete
    | 0, j -> List.init j (fun j -> insert j j)
    | i, j ->
        let next_coord, action =
          if grid.(i).(j) = grid.(i - 1).(j - 1) then ((i - 1, j - 1), [])
          else
            ( ((i - 1, j), [ delete (i - 1) ]),
              ((i, j - 1), [ insert i (j - 1) ]),
              ((i - 1, j - 1), [ substitute (i - 1) (j - 1) ]) )
            |> Triple.minimum_on ~compare:Int.compare (fun ((i, j), _) ->
                   grid.(i).(j))
        in
        (aux [@tailcall]) (action @ acc) next_coord
  in
  aux [] (Array.length grid - 1, Array.length grid.(0) - 1)

let levenshtein_dp ~equal a b =
  let grid = construct_grid ~equal a b in
  reconstruct_edit_script grid
  (* Map the command indices to the true coordinates *)
  |> List.map (map_expected (( + ) a.origin) >> map_actual (( + ) b.origin))

(** Strip common prefixes and suffixes of the input sequences can be stripped
    (in linear time) to avoid running the full dynamic programming algorithm on
    them. *)
let strip_common_outer (type a) ~equal ((a : a subarray), (b : a subarray)) =
  let rec modify_until_nonequal op (i, j) =
    let oob { origin; length; _ } i = i < origin || i >= origin + length in
    let a_oob = oob a i and b_oob = oob b j in

    if a_oob && b_oob then `Equal (* Finished scanning both arrays *)
    else if a_oob || b_oob || not (equal a.array.(i) b.array.(j)) then
      `Non_equal (i, j)
    else modify_until_nonequal op (op i, op j)
  in
  match modify_until_nonequal (( + ) 1) (a.origin, b.origin) with
  | `Equal -> `Equal (* The arrays are equal *)
  | `Non_equal (a_origin, b_origin) -> (
      match
        modify_until_nonequal (( + ) (-1))
          (a.origin + a.length - 1, b.origin + b.length - 1)
      with
      | `Equal ->
          assert false (* We already decided that the arrays are non-equal *)
      | `Non_equal (a_last, b_last) ->
          `Non_equal
            ( { a with origin = a_origin; length = a_last - a_origin + 1 },
              { b with origin = b_origin; length = b_last - b_origin + 1 } ) )

let levenshtein_script (type a) ~(equal : a -> a -> bool) (a : a array)
    (b : a array) : a edit_script =
  let a = { array = a; origin = 0; length = Array.length a }
  and b = { array = b; origin = 0; length = Array.length b } in

  match strip_common_outer ~equal (a, b) with
  | `Equal -> []
  | `Non_equal (a, b) -> levenshtein_dp ~equal a b
