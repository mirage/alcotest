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

module Core = Core
module Cli = Cli
module Monad = Monad
module T = Cli.Make (Monad.Identity)
include T

module type TESTABLE = sig
  type t

  val pp : t Fmt.t

  val equal : t -> t -> bool
end

type 'a testable = (module TESTABLE with type t = 'a)

let pp (type a) (t : a testable) =
  let (module T) = t in
  T.pp

let equal (type a) (t : a testable) =
  let (module T) = t in
  T.equal

let isnan f = FP_nan = classify_float f

let testable (type a) (pp : a Fmt.t) (equal : a -> a -> bool) : a testable =
  let module M = struct
    type t = a

    let pp = pp

    let equal = equal
  end in
  (module M)

let int32 = testable Fmt.int32 ( = )

let int64 = testable Fmt.int64 ( = )

let int = testable Fmt.int ( = )

let float eps =
  let same x y =
    (isnan x && isnan y)
    (* compare infinities *)
    || x = y
    || abs_float (x -. y) <= eps
  in
  testable Fmt.float same

let char = testable Fmt.char ( = )

let string = testable Fmt.string ( = )

let bool = testable Fmt.bool ( = )

let unit = testable (Fmt.unit "()") ( = )

let list e =
  let rec eq l1 l2 =
    match (l1, l2) with
    | x :: xs, y :: ys -> equal e x y && eq xs ys
    | [], [] -> true
    | _ -> false
  in
  testable (Fmt.Dump.list (pp e)) eq

let slist (type a) (a : a testable) compare =
  let l = list a in
  let eq l1 l2 = equal l (List.sort compare l1) (List.sort compare l2) in
  testable (pp l) eq

let array e =
  let eq a1 a2 =
    let m, n = Array.(length a1, length a2) in
    let rec go i = i = m || (equal e a1.(i) a2.(i) && go (i + 1)) in
    m = n && go 0
  in
  testable (Fmt.Dump.array (pp e)) eq

let pair a b =
  let eq (a1, b1) (a2, b2) = equal a a1 a2 && equal b b1 b2 in
  testable (Fmt.Dump.pair (pp a) (pp b)) eq

let option e =
  let eq x y =
    match (x, y) with
    | Some a, Some b -> equal e a b
    | None, None -> true
    | _ -> false
  in
  testable (Fmt.Dump.option (pp e)) eq

let result a e =
  let eq x y =
    match (x, y) with
    | Ok x, Ok y -> equal a x y
    | Error x, Error y -> equal e x y
    | _ -> false
  in
  testable (Fmt.Dump.result ~ok:(pp a) ~error:(pp e)) eq

let of_pp pp = testable pp ( = )

let pass (type a) =
  let module M = struct
    type t = a

    let pp fmt _ = Fmt.string fmt "Alcotest.pass"

    let equal _ _ = true
  end in
  (module M : TESTABLE with type t = M.t)

let reject (type a) =
  let module M = struct
    type t = a

    let pp fmt _ = Fmt.string fmt "Alcotest.reject"

    let equal _ _ = false
  end in
  (module M : TESTABLE with type t = M.t)

let show_assert msg =
  Format.eprintf "%a %s\n" Fmt.(styled `Yellow string) "ASSERT" msg

let check_err fmt =
  Format.ksprintf (fun err -> raise (Core.Check_error err)) fmt

let check t msg x y =
  show_assert msg;
  if not (equal t x y) then
    Fmt.strf "Error %s: expecting@\n%a, got@\n%a." msg (pp t) x (pp t) y
    |> failwith

let fail msg =
  show_assert msg;
  check_err "Error %s." msg

let failf fmt = Fmt.kstrf fail fmt

let neg t = testable (pp t) (fun x y -> not (equal t x y))

let collect_exception f =
  try
    f ();
    None
  with e -> Some e

let check_raises msg exn f =
  show_assert msg;
  match collect_exception f with
  | None ->
      check_err "Fail %s: expecting %s, got nothing." msg
        (Printexc.to_string exn)
  | Some e ->
      if e <> exn then
        check_err "Fail %s: expecting %s, got %s." msg (Printexc.to_string exn)
          (Printexc.to_string e)

let () = at_exit (Format.pp_print_flush Format.err_formatter)
