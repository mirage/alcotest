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

module Subarray : sig
  type 'a t
  (** Read-only wrapper around an array or a string. Can be {!truncate}d in
      [O(1)] time. *)

  val truncate : origin:int -> length:int -> 'a t -> 'a t
  (** Return a new subarray with smaller bounds than the previous one. *)

  val get : 'a t -> int -> 'a

  val length : 'a t -> int

  val origin : 'a t -> int

  val of_array : 'a array -> 'a t

  val of_list : 'a list -> 'a t

  val of_string : string -> char t
end = struct
  type 'a t = { get : int -> 'a; origin : int; length : int }

  let truncate ~origin ~length
      { get; origin = prev_origin; length = prev_length } =
    if origin < prev_origin || length > prev_length then
      invalid_arg "Cannot expand array during truncation";
    { get; origin; length }

  let get { get; origin; length } i =
    if i >= length then Fmt.invalid_arg "index out of bounds: %d" i;
    get (i + origin)

  let length { length; _ } = length

  let origin { origin; _ } = origin

  let of_array a = { get = Array.get a; origin = 0; length = Array.length a }

  let of_list l = Array.of_list l |> of_array

  let of_string s = { get = String.get s; origin = 0; length = String.length s }
end

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
let construct_grid (type a) ~(equal : a -> a -> bool) (a : a Subarray.t)
    (b : a Subarray.t) : int array array =
  let grid_x_length = Subarray.length a + 1
  and grid_y_length = Subarray.length b + 1 in
  let grid = Array.make_matrix grid_x_length grid_y_length 0 in
  let get_grid (i, j) = grid.(i).(j) in

  for i = 0 to grid_x_length - 1 do
    for j = 0 to grid_y_length - 1 do
      let cost =
        if min i j = 0 then max i j
        else if equal (Subarray.get a (i - 1)) (Subarray.get b (j - 1)) then
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
  |> List.map
       ( map_expected (( + ) (Subarray.origin a))
       >> map_actual (( + ) (Subarray.origin b)) )

(** Strip common prefixes and suffixes of the input sequences can be stripped
    (in linear time) to avoid running the full dynamic programming algorithm on
    them. *)
let strip_common_outer (type a) ~equal ((a : a Subarray.t), (b : a Subarray.t))
    =
  let len_a = Subarray.length a and len_b = Subarray.length b in

  (* Keep shifting the indices until they point to non-equal values in the
     arrays (or we scan an entire array). *)
  let rec modify_until_nonequal op (i, j) =
    let a_oob = i < 0 || i >= len_a and b_oob = j < 0 || j >= len_b in
    if a_oob && b_oob then `Equal (* Finished scanning both arrays *)
    else if a_oob || b_oob || not (equal (Subarray.get a i) (Subarray.get b j))
    then `Non_equal (i, j)
    else modify_until_nonequal op (op i, op j)
  in

  match modify_until_nonequal (( + ) 1) (0, 0) with
  | `Equal -> `Equal (* The arrays are equal *)
  | `Non_equal (a_origin, b_origin) -> (
      match modify_until_nonequal (( + ) (-1)) (len_a - 1, len_b - 1) with
      | `Equal ->
          assert false (* We already decided that the arrays are non-equal *)
      | `Non_equal (a_last, b_last) ->
          `Non_equal
            ( Subarray.truncate ~origin:a_origin
                ~length:(a_last - a_origin + 1)
                a,
              Subarray.truncate ~origin:b_origin
                ~length:(b_last - b_origin + 1)
                b ) )

type ('a, _) typ =
  | Array : ('a, 'a array) typ
  | List : ('a, 'a list) typ
  | String : (char, string) typ

let levenshtein_script (type a container) (typ : (a, container) typ)
    ~(equal : a -> a -> bool) (a : container) (b : container) : a edit_script =
  let a, b =
    match typ with
    | Array -> Subarray.(of_array a, of_array b)
    | List -> Subarray.(of_list a, of_list b)
    | String -> Subarray.(of_string a, of_string b)
  in
  match strip_common_outer ~equal (a, b) with
  | `Equal -> []
  | `Non_equal (a, b) -> levenshtein_dp ~equal a b
