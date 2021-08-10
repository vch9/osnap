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

module Spec = Osnap__Spec
module M = Osnap__Memory
module Test = Osnap.Test
module Snapshot = Osnap.Snapshot

let test_create_snapshot_one () =
  let rand = Random.State.make [| 42; 9 |] in

  let spec = Spec.(int ^> int ^>> string_of_int) in
  let test = Test.(make ~count:1 ~path:"" ~name:"foo" ~spec ( + )) in
  let snapshot = Snapshot.make ~rand test in
  let decoded_snapshot = M.Snapshot.decode spec snapshot in

  let expected =
    {|{ name = "foo";
  applications =
  [["3306656436478733947"; "2323438535601724629"; "-3593277064774317232"]] }|}
  in
  let actual = M.Snapshot.show decoded_snapshot in

  Alcotest.(check string) "create snapshot" expected actual

let test_create_snapshot_two () =
  let rand = Random.State.make [| 42; 9 |] in

  let spec = Spec.(int ^> int ^>> string_of_int) in
  let test = Test.(make ~count:2 ~path:"" ~name:"foo" ~spec ( + )) in
  let snapshot = Snapshot.make ~rand test in
  let decoded_snapshot = M.Snapshot.decode spec snapshot in

  let expected =
    {|{ name = "foo";
  applications =
  [["3306656436478733947"; "2323438535601724629"; "-3593277064774317232"];
    ["-1045094426214325490"; "-2812697657021115463"; "-3857792083235440953"]]
  }|}
  in
  let actual = M.Snapshot.show decoded_snapshot in

  Alcotest.(check string) "create snapshot" expected actual

let test_fancy_show () =
  let rand = Random.State.make [| 42; 9 |] in

  let small_int =
    Spec.{ gen = QCheck.Gen.small_int; printer = string_of_int }
  in
  let spec = Spec.(small_int ^> small_int ^>> string_of_int) in

  let test = Test.(make ~count:5 ~path:"" ~name:"add" ~spec ( + )) in
  let snapshot = Snapshot.make ~rand test in

  let expected =
    {|add 37 4 = 41
add 56 45 = 101
add 5 3 = 8
add 8 67 = 75
add 66 55 = 121
|}
  in
  let actual = Snapshot.show @@ M.Snapshot.decode spec snapshot in

  Alcotest.(check string) "fancy show" expected actual

let tests =
  ( "Osnap",
    Alcotest.
      [
        test_case "create snapshot" `Quick test_create_snapshot_one;
        test_case "create snapshot" `Quick test_create_snapshot_two;
        test_case "show fancy snapshot" `Quick test_fancy_show;
      ] )
