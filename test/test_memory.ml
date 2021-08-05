(*****************************************************************************)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Valentin Chaboche                                      *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Osnap__Memory

let test_create_osnap () =
  let () = create_osnap "./" in
  let expected_file = ".osnap/.keep" in
  Alcotest.(check bool)
    "create_osnap ./; file_exists .osnap/.keep"
    true
    (Sys.file_exists expected_file)

let test_write_read () =
  let content = "foo" in
  let path = "./foo" in
  let () = write_osnap path content in
  let content' = read_osnap path in
  Alcotest.(check string) "write path s; read path = s" content content'

let test_write_read_lines () =
  let content = {|
    add 0 0 = 0;
    add 1 1 = 2; |} in
  let path = "./foo" in
  let () = write_osnap path content in
  let content' = read_osnap path in
  Alcotest.(check string) "write path s; read path = s" content content'

let tests =
  ( "Memory",
    Alcotest.
      [
        test_case "test build_to_src_path ./" `Quick test_create_osnap;
        test_case "test write_read ./" `Quick test_write_read;
        test_case "test write_read ./" `Quick test_write_read_lines;
      ] )
