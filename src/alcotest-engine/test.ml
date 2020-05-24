(*
 * Copyright (c) 2013-2016 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Utils

module Diff = struct
  type t = { expected : unit Fmt.t; actual : unit Fmt.t }
  (** Diffs can be rendered as a 'merged' output that compares the two input
      values in some way, or as two distinct outputs: one for each input. *)

  let pp { expected; actual } : unit Fmt.t =
    let open Fmt in
    let s = const string in
    let pp_expected =
      s "   Expected: "
      ++ styled `Faint expected
      ++ Format.pp_print_if_newline
      ++ cut
    and pp_actual = s "   Received: " ++ styled `Faint actual in
    vbox (pp_expected ++ cut ++ pp_actual)
end

type check_result = {
  diff : Diff.t;
      (** We keep separate pretty-printers for both values even in the [Pass]
          case, since they might still pretty-print differently. *)
  result : [ `Pass | `Fail of [ `Total | `Partial ] ];
}

type 'a check = expected:'a -> actual:'a -> check_result
(** An equality check extended with the ability to pretty-print diffs. *)

type 'a testable =
  | Testable : { pp : 'a Fmt.t; equal : 'a check } -> 'a testable

let testable (type a) (pp : a Fmt.t) (equal : a check) : a testable =
  Testable { pp; equal }

let testable' (type a) (pp : a Fmt.t) (equal : a -> a -> bool) : a testable =
  let equal ~expected ~actual =
    let result =
      match equal expected actual with true -> `Pass | false -> `Fail `Total
    in
    {
      result;
      diff =
        { expected = Fmt.(const pp expected); actual = Fmt.(const pp actual) };
    }
  in
  Testable { pp; equal }

let pp (type a) (Testable { pp; _ } : a testable) : a Fmt.t = pp

let eq (type a) (Testable { equal; _ } : a testable) : a check = equal

let equal (type a) (t : a testable) : a -> a -> bool =
 fun a b ->
  match (eq t ~expected:a ~actual:b).result with
  | `Pass -> true
  | `Fail _ -> false

let isnan f = FP_nan = classify_float f

let int32 = testable' Fmt.int32 ( = )

let int64 = testable' Fmt.int64 ( = )

let int = testable' Fmt.int ( = )

let float eps =
  let same x y =
    (isnan x && isnan y)
    (* compare infinities *)
    || x = y
    || abs_float (x -. y) <= eps
  in
  testable' Fmt.float same

let char = testable' Fmt.char ( = )

let bool = testable' Fmt.bool ( = )

let unit = testable' (Fmt.unit "()") ( = )

let map_indices : type a. int list -> (a -> a) -> a list -> a list =
 fun indices f ->
  let rec inner current indices l =
    match (indices, l) with
    | [], _ -> l
    | n :: _, [] -> Fmt.invalid_arg "map_indices: index %d out-of-bounds" n
    | i :: is, x :: xs when i = current -> f x :: inner (current + 1) is xs
    | _, x :: xs -> x :: inner (current + 1) indices xs
  in
  inner 0 indices

let highlight_expected = Fmt.styled (`Fg (`Hi `Green))

let highlight_actual = Fmt.styled (`Fg (`Hi `Red))

let check_result_of_edit_script (type a) ~(pp_elt : a Fmt.t)
    ~(combine : unit Fmt.t list -> unit Fmt.t) (l1 : a list) (l2 : a list)
    (edits : a Distance.edit_script) : check_result =
  (* If the edit distance is too high relative to the size of the inputs, we are
     trying too hard to make them match & adding noise to the output. Beyond a
     certain arbitrary threshold, we just report a total failure. *)
  let report_total_failure =
    let edit_ratio =
      let len x = List.length x |> Float.of_int in
      len edits /. max (len l1) (len l2)
    in
    let threshold = (4. /. 5.) -. Float.epsilon in
    edit_ratio > threshold
  in
  match report_total_failure with
  | true ->
      let expected = Fmt.(const (Dump.list pp_elt)) l1
      and actual = Fmt.(const (Dump.list pp_elt)) l2 in
      { result = `Fail `Total; diff = { expected; actual } }
  | false ->
      let expected_to_highlight, actual_to_highlight =
        edits
        |> List.fold_left
             (fun (lefts, rights) -> function
               | Distance.Insert { actual; _ } -> (lefts, actual :: rights)
               | Delete { expected } -> (expected :: lefts, rights)
               | Substitute { expected; actual } ->
                   (expected :: lefts, actual :: rights))
             ([], [])
        |> fun (lefts, rights) -> (List.rev lefts, List.rev rights)
      in
      let expected, actual =
        ( (l1, expected_to_highlight, highlight_expected),
          (l2, actual_to_highlight, highlight_actual) )
        |> Pair.map (fun (list, to_highlight, highlight) ->
               list
               |> List.map Fmt.(const (box pp_elt))
               |> map_indices to_highlight highlight
               |> combine)
      in
      { result = `Fail `Partial; diff = { expected; actual } }

(* Types with diffs computed by levenshtein distance. *)

let list (type a) (elt : a testable) : a list testable =
  let elt_equal = equal elt in
  let pp_elt = pp elt in
  let pp = Fmt.Dump.list (pp elt) in
  let equal ~expected ~actual =
    (* Avoid cost of computing Levenshtein distance in the common case of the
       check succeeding. *)
    match List.equal elt_equal expected actual with
    | true ->
        let expected = Fmt.(const pp) expected
        and actual = Fmt.(const pp) actual in
        { result = `Pass; diff = { expected; actual } }
    | false -> (
        match
          Distance.(levenshtein_script List) ~equal:elt_equal expected actual
        with
        | [] ->
            assert false (* Levenshtein distance of zero for non-equal lists. *)
        | _ :: _ as edits ->
            check_result_of_edit_script ~pp_elt
              ~combine:Fmt.(concat ~sep:semi >> brackets)
              expected actual edits )
  in
  testable pp equal

let slist (type a) (a : a testable) compare =
  let l = list a in
  let eq l1 l2 = equal l (List.sort compare l1) (List.sort compare l2) in
  testable' (pp l) eq

let array (type a) (elt : a testable) : a array testable =
  let oxford_brackets pp_v =
    Fmt.(box ~indent:2 (const string "[|" ++ pp_v ++ const string "|]"))
  in
  let elt_equal = equal elt in
  let pp_elt = pp elt in
  let pp = Fmt.Dump.array (pp elt) in
  let equal ~expected ~actual =
    match Array.equal elt_equal expected actual with
    | true ->
        let expected = Fmt.const pp expected and actual = Fmt.const pp actual in
        { result = `Pass; diff = { expected; actual } }
    | false -> (
        match
          Distance.(levenshtein_script Array) ~equal:elt_equal expected actual
        with
        | [] ->
            assert false (* Levenshtein distance of zero for non-equal arrays *)
        | _ :: _ as edits ->
            check_result_of_edit_script ~pp_elt
              ~combine:Fmt.(concat ~sep:semi >> oxford_brackets)
              (Array.to_list expected) (Array.to_list actual) edits )
  in
  testable pp equal

let string =
  let equal ~expected ~actual =
    match String.equal expected actual with
    | true ->
        let expected = Fmt.(const string) expected
        and actual = Fmt.(const string) actual in
        { result = `Pass; diff = { expected; actual } }
    | false -> (
        match
          Distance.(levenshtein_script String) ~equal:Char.equal expected actual
        with
        | [] ->
            assert false (* Levenshtein distance of zero for non-equal arrays *)
        | _ :: _ as edits ->
            check_result_of_edit_script ~pp_elt:Fmt.char
              ~combine:Fmt.(concat ~sep:nop)
              (expected |> String.to_seq |> Seq.to_list)
              (actual |> String.to_seq |> Seq.to_list)
              edits )
  in

  testable Fmt.string equal

let merge_results : check_result * check_result -> check_result =
 fun (res_a, res_b) ->
  let diff =
    let pp_pair a b = Fmt.(parens (box a ++ comma ++ box b)) in
    let expected = pp_pair res_a.diff.expected res_b.diff.expected
    and actual = pp_pair res_a.diff.actual res_b.diff.actual in
    Diff.{ expected; actual }
  in
  let result =
    match (res_a.result, res_b.result) with
    | `Pass, `Pass -> `Pass
    | `Fail `Total, `Fail `Total -> `Fail `Total
    | `Fail _, _ | _, `Fail _ -> `Fail `Partial
  in
  { diff; result }

let pair (type a b) (a : a testable) (b : b testable) : (a * b) testable =
  let equal ~expected:(a_expected, b_expected) ~actual:(a_actual, b_actual) =
    merge_results
      ( eq a ~expected:a_expected ~actual:a_actual,
        eq b ~expected:b_expected ~actual:b_actual )
  in
  let pp = Fmt.Dump.pair (pp a) (pp b) in
  testable pp equal

let option e =
  let eq x y =
    match (x, y) with
    | Some a, Some b -> equal e a b
    | None, None -> true
    | _ -> false
  in
  testable' (Fmt.Dump.option (pp e)) eq

let result a e =
  let eq x y =
    match (x, y) with
    | Ok x, Ok y -> equal a x y
    | Error x, Error y -> equal e x y
    | _ -> false
  in
  testable' (Fmt.Dump.result ~ok:(pp a) ~error:(pp e)) eq

let of_pp pp = testable' pp ( = )

let pass (type a) : a testable =
  let pp fmt _ = Fmt.string fmt "Alcotest.pass"
  and equal ~expected:_ ~actual:_ =
    {
      result = `Pass;
      diff =
        { expected = Fmt.(const string "__"); actual = Fmt.(const string "__") };
    }
  in
  Testable { pp; equal }

let reject (type a) : a testable =
  let pp fmt _ = Fmt.string fmt "Alcotest.reject"
  and equal ~expected:_ ~actual:_ =
    {
      result = `Fail `Total;
      diff =
        { expected = Fmt.(const string "__"); actual = Fmt.(const string "__") };
    }
  in
  Testable { pp; equal }

let show_assert msg =
  Fmt.(flush stdout) () (* Flush any test stdout preceding the assert *);
  Format.eprintf "%a %s\n%!" Pp.tag `Assert msg

let check_err fmt = raise (Core.Check_error fmt)

let check (type a) (Testable { equal; pp } : a testable) msg (expected : a)
    (actual : a) =
  show_assert msg;
  let { result; diff } = equal ~expected ~actual in
  match result with
  | `Pass -> ()
  | `Fail f ->
      let diff =
        match f with
        | `Partial -> diff
        | `Total ->
            {
              expected = highlight_expected (Fmt.const pp expected);
              actual = highlight_actual (Fmt.const pp actual);
            }
      in

      let pp_error = Fmt.(const Pp.tag `Error ++ const string (" " ^ msg)) in
      let merged = Diff.(pp diff) in
      raise (Core.Check_error Fmt.(pp_error ++ cut ++ cut ++ merged))

let check' t ~msg ~expected ~actual = check t msg expected actual

let fail msg =
  show_assert msg;
  check_err (fun ppf () -> Fmt.pf ppf "%a %s" Pp.tag `Fail msg)

let failf fmt = Fmt.kstrf fail fmt

let neg t = testable' (pp t) (fun x y -> not (equal t x y))

let collect_exception f =
  try
    f ();
    None
  with e -> Some e

let check_raises msg exn f =
  show_assert msg;
  match collect_exception f with
  | None ->
      check_err (fun ppf () ->
          Fmt.pf ppf "%a %s: expecting %s, got nothing." Pp.tag `Fail msg
            (Printexc.to_string exn))
  | Some e ->
      if e <> exn then
        check_err (fun ppf () ->
            Fmt.pf ppf "%a %s: expecting %s, got %s." Pp.tag `Fail msg
              (Printexc.to_string exn) (Printexc.to_string e))

let () = at_exit (Format.pp_print_flush Format.err_formatter)

module type TESTABLE = sig
  type t

  val pp : t Fmt.t

  val equal : t -> t -> bool
end

let testable_of_module (type a) (module T : TESTABLE with type t = a) =
  testable' T.pp T.equal

let testable = testable'
