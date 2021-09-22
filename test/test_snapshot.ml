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

module S = Osnap__Snapshot
open Test_helpers

let eq spec = Alcotest.of_pp (fun fmt -> S.pp fmt spec)

let pp spec fmt = S.pp fmt spec

let arb_snapshot =
  QCheck.(
    map
      (fun (name, n) -> S.create ~rand ~name ~spec:spec_add ~f:add n)
      (pair string small_int))

let test_create =
  QCheck.Test.make
    ~name:"create name n <=> snap.name = name && |snap.scenarios| = n"
    QCheck.(pair string small_int)
    (fun (name, n) ->
      let (S.Snapshot { name = name'; scenarios }) =
        S.create ~rand ~name ~spec:spec_add ~f:add n
      in
      name = name' && List.length scenarios = n)

let test_encode_marshal =
  QCheck.Test.make
    ~name:"fun snapshot -> decode (encode snapshot) = snapshot with Marshal"
    arb_snapshot
    (fun snapshot ->
      let path = "./foo" in
      let expected = snapshot in
      let actual =
        S.encode ~spec:spec_add ~mode:`Binary ~path snapshot ;
        S.decode ~spec:spec_add ~mode:`Binary ~path ()
      in
      qcheck_eq ~pp:(pp spec_add) expected actual)

let test_encode_json =
  QCheck.Test.make
    ~name:
      "fun snapshot -> decode (encode snapshot) = snapshot with Data_encoding"
    arb_snapshot
    (fun snapshot ->
      let path = "./foo" in
      let expected = snapshot in
      let actual =
        S.encode ~spec:spec_add ~mode:`Encoding ~path snapshot ;
        S.decode ~spec:spec_add ~mode:`Encoding ~path ()
      in
      qcheck_eq ~pp:(pp spec_add) expected actual)

let test_fail_json_invalid_spec () =
  let incomplete_spec =
    Osnap.Spec.(int ^> of_gen QCheck.Gen.int ^>> build_result string_of_int)
  in
  let path = "./foo" in
  let snapshot = S.create ~rand ~name:"" ~spec:incomplete_spec ~f:add 5 in

  Alcotest.check_raises
    "encode with incomplete encodings must fail"
    (Invalid_argument "Some encoding fields in the specification are missing")
    (fun () -> S.encode ~spec:incomplete_spec ~mode:`Encoding ~path snapshot)

let test_fail_json_no_spec_encode () =
  let path = "./foo" in
  let snapshot = S.create ~rand ~name:"" ~spec:spec_add ~f:add 5 in

  Alcotest.check_raises
    "encode with no specification must fail"
    (Invalid_argument "Cannot encode a snapshot without the specification")
    (fun () -> S.encode ~mode:`Encoding ~path snapshot)

let test_fail_json_no_spec_decode () =
  let path = "./foo" in
  let snapshot = S.create ~rand ~name:"" ~spec:spec_add ~f:add 5 in

  Alcotest.check_raises
    "encode with no specification must fail"
    (Invalid_argument "Cannot decode a snapshot without the specification")
    (fun () ->
      S.encode ~spec:spec_add ~mode:`Encoding ~path snapshot ;
      S.decode ~mode:`Encoding ~path () |> ignore)

let test_create_empty () =
  let actual = S.create ~rand ~name:"empty" ~spec:spec_add ~f:add 0 in
  let expected = S.(Snapshot { name = "empty"; scenarios = [] }) in
  Alcotest.(check (eq spec_add)) "create ~name:empty _ _ 0" expected actual

let test_to_string () =
  let snapshot = S.create ~rand ~name:"add" ~spec:spec_add ~f:add 2 in
  let expected =
    {|{
  name = add;
  scenarios = [
	8    2    =    10
	68    4    =    72
  ]
}|}
  in
  let actual = S.to_string spec_add snapshot in

  Alcotest.(check string) "test to_string" expected actual

let qcheck_tests =
  [ test_create; test_encode_marshal; test_encode_json ]
  |> List.map (QCheck_alcotest.to_alcotest ~rand)

let tests =
  Alcotest.
    [
      test_case "test_create_empty" `Quick test_create_empty;
      test_case "test_fail_json_invalid_spec" `Quick test_fail_json_invalid_spec;
      test_case
        "test_fail_json_no_spec_encode"
        `Quick
        test_fail_json_no_spec_encode;
      test_case
        "test_fail_json_no_spec_decode"
        `Quick
        test_fail_json_no_spec_decode;
      test_case "test_to_string" `Quick test_to_string;
    ]

let tests = ("Snapshot", qcheck_tests @ tests)
