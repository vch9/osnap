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

module S = Osnap__Scenario
open Test_helpers

let eq spec = Alcotest.of_pp (fun fmt -> S.pp fmt spec)

let test_reapply_good () =
  let scenario = S.(Cons (1, Cons (1, Res (Ok 2)))) in
  let actual = S.reapply scenario add in
  Alcotest.(check (eq spec_add))
    "reapply with same f must returns the same scenario"
    scenario
    actual

let test_pp () =
  let pp fmt = S.pp fmt spec_add in
  let scenario = S.(Cons (0, Cons (1, Res (Ok 1)))) in
  let expected = Printf.sprintf "0\t1\t=\t1" in
  let actual = Format.asprintf "%a" pp scenario in
  Alcotest.(check string) "test pp scenario" expected actual

let test_raises_exception () =
  let spec = Osnap.Spec.(small_int ^>> build_result string_of_int) in
  let f _ = failwith "error" in
  let actual = S.spec_to_scenario ~rand spec f in
  let expected = S.(Cons (5, Res (Error "(Failure error)"))) in
  Alcotest.check (eq spec) "exception is catched" expected actual

let test_reapply_exception () =
  let spec = Osnap.Spec.(small_int ^>> build_result string_of_int) in
  let scenario = S.(Cons (5, Res (Error "(Failure error)"))) in
  let f _ = failwith "error" in
  let actual = S.reapply scenario f in
  Alcotest.check (eq spec) "reapply with exceptions" scenario actual

let tests =
  ( "Scenario",
    Alcotest.
      [
        test_case
          "test_reapply scenario on equivalent new f"
          `Quick
          test_reapply_good;
        test_case "test pp scenario" `Quick test_pp;
        test_case "test exception catched" `Quick test_raises_exception;
        test_case "test reapply with exceptions" `Quick test_reapply_exception;
      ] )
