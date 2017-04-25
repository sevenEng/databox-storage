#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "databox-storage" @@ fun c ->
  Ok [ Pkg.mllib "src/databox-storage.mllib";
       Pkg.test "test/test_store";
       (*Pkg.test "test/test_log";*) ]
