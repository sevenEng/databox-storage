#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "databox-storage" @@ fun c ->
  Ok [ Pkg.mllib "src/databox-storage.mllib";
       Pkg.bin "bin/server";
       Pkg.test "test/test_log"; ]
