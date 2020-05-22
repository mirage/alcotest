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

type output = unit Fmt.t
(** Staged pretty-printed output waiting on styling details supplied elsewhere. *)

module Diff = struct
  (** Diffs can be rendered as a 'merged' output that compares the two input
      values in some way, or as two distinct outputs: one for each input. *)
  type t = Diff of { expected : output; actual : output }
end

let merge_diffs ((expected : output), (actual : output)) : output =
  let open Fmt in
  let s = const string in
  let pp_expected =
    s "   Expected: "
    ++ styled `Faint expected
    ++ Format.pp_print_if_newline
    ++ cut
  and pp_actual = s "   Received: " ++ styled `Faint actual in
  vbox (pp_expected ++ cut ++ pp_actual)

type check_result = Pass | Fail of Diff.t

type 'a check = 'a -> 'a -> check_result
(** An equality check extended with the ability to pretty-print diffs in the
    failure case. *)

type 'a testable =
  | Testable : { pp : 'a Fmt.t; equal : 'a check } -> 'a testable

let testable (type a) (pp : a Fmt.t) (equal : a check) : a testable =
  Testable { pp; equal }

let testable' (type a) (pp : a Fmt.t) (equal : a -> a -> bool) : a testable =
  let equal a b =
    match equal a b with
    | true -> Pass
    | false ->
        Fail (Diff { expected = Fmt.(const pp a); actual = Fmt.(const pp b) })
  in
  Testable { pp; equal }

let pp (type a) (Testable { pp; _ } : a testable) : a Fmt.t = pp

let eq (type a) (Testable { equal; _ } : a testable) : a check = equal

let equal (type a) (t : a testable) : a -> a -> bool =
 fun a b -> match eq t a b with Pass -> true | Fail _ -> false

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

let string = testable' Fmt.string ( = )

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

let diff_of_edit_script (type a) ~(pp_elt : a Fmt.t)
    ~(combine : unit Fmt.t list -> unit Fmt.t) (l1 : a list) (l2 : a list)
    (edits : a Distance.edit_script) : Diff.t =
  let highlight_left, highlight_right =
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
    ((l1, highlight_left, `Green), (l2, highlight_right, `Red))
    |> Pair.map (fun (list, to_highlight, highlight) ->
           list
           |> List.map Fmt.(const (box pp_elt))
           |> map_indices to_highlight (Fmt.styled (`Fg (`Hi highlight)))
           |> combine)
  in
  Diff { expected; actual }

(* Types with diffs computed by levenshtein distance. *)

let list (type a) (elt : a testable) : a list testable =
  let elt_equal = equal elt in
  let equal l1 l2 =
    (* Avoid cost of computing Levenshtein distance in the common case of the
       check succeeding. *)
    match List.equal elt_equal l1 l2 with
    | true -> Pass
    | false -> (
        match Distance.levenshtein_script ~equal:elt_equal l1 l2 with
        | [] ->
            assert false (* Levenshtein distance of zero for non-equal lists. *)
        | _ :: _ as edits ->
            Fail
              (diff_of_edit_script ~pp_elt:(pp elt)
                 ~combine:Fmt.(concat ~sep:semi >> brackets)
                 l1 l2 edits) )
  in
  let pp = Fmt.Dump.list (pp elt) in
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
  let equal a1 a2 =
    match Array.equal elt_equal a1 a2 with
    | true -> Pass
    | false -> (
        let l1, l2 = Pair.map Array.to_list (a1, a2) in
        match Distance.levenshtein_script ~equal:elt_equal l1 l2 with
        | [] ->
            assert false (* Levenshtein distance of zero for non-equal arrays *)
        | _ :: _ as edits ->
            Fail
              (diff_of_edit_script ~pp_elt:(pp elt)
                 ~combine:Fmt.(concat ~sep:semi >> oxford_brackets)
                 l1 l2 edits) )
  in
  let pp = Fmt.Dump.array (pp elt) in
  testable pp equal

let pair a b =
  let eq (a1, b1) (a2, b2) = equal a a1 a2 && equal b b1 b2 in
  testable' (Fmt.Dump.pair (pp a) (pp b)) eq

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
  let pp fmt _ = Fmt.string fmt "Alcotest.pass" and equal _ _ = Pass in
  Testable { pp; equal }

let reject (type a) : a testable =
  let pp fmt _ = Fmt.string fmt "Alcotest.reject"
  and equal _ _ =
    Fail
      (Diff.Diff
         {
           expected = Fmt.(const string) "__";
           actual = Fmt.(const string) "__";
         })
  in
  Testable { pp; equal }

let show_assert msg =
  Fmt.(flush stdout) () (* Flush any test stdout preceding the assert *);
  Format.eprintf "%a %s\n%!" Pp.tag `Assert msg

let check_err fmt = raise (Core.Check_error fmt)

let check (type a) (Testable { equal; _ } : a testable) msg (expected : a)
    (actual : a) =
  show_assert msg;
  match equal expected actual with
  | Pass -> ()
  | Fail (Diff { expected; actual }) ->
      let pp_error = Fmt.(const Pp.tag `Error ++ const string (" " ^ msg)) in
      let merged = merge_diffs (expected, actual) in
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
