#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "alcotest" @@ fun c ->
  Ok [
    Pkg.mllib "src/alcotest.mllib";
    Pkg.test "examples/simple";
    Pkg.test ~run:false "examples/bad";
  ]
