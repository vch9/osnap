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

module M = Osnap__Memory

let test_encode_empty () =
  let x = M.Snapshot.build "empty" [] in
  let expected = {|{ name = "empty"; applications = [] }|} in
  let actual = M.Snapshot.show x in
  Alcotest.(check string)
    "build empty [] = { name = empty; applications = [] }"
    expected
    actual

let test_encode_nonempty () =
  let x = M.Snapshot.build "foo" [ [ "0"; "0"; "0" ]; [ "2"; "2"; "2" ] ] in
  let expected =
    {|{ name = "foo"; applications = [["0"; "0"; "0"]; ["2"; "2"; "2"]] }|}
  in
  let actual = M.Snapshot.show x in
  Alcotest.(check string)
    "build empty [] = { name: foo, applications = [[0;0;0];[2;2;4]] }"
    expected
    actual

let test_encode_marshal () =
  let arg0 = M.Encode.to_string 0 [] in
  let arg1 = M.Encode.to_string 1 [] in
  let x = M.Snapshot.build "foo" [ [ arg0; arg1 ] ] in
  let expected =
    {|{ name = "foo";
  applications =
  [["\132\149\166\190\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000@";
     "\132\149\166\190\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000A"
     ]
    ]
  }|}
  in
  let actual = M.Snapshot.show x in
  Alcotest.(check string) "build with encoded int and string" expected actual

let test_encode_args () =
  let open Osnap__Interpreter in
  let args = Cons (0, Cons ("0", Nil)) in
  let arg0 = M.Encode.to_string args [] in
  let x = M.Snapshot.build "foo" [ [ arg0 ] ] in
  let expected =
    {|{ name = "foo";
  applications =
  [["\132\149\166\190\000\000\000\006\000\000\000\003\000\000\000\b\000\000\000\b\160@\160!0@"
     ]
    ]
  }|}
  in
  let actual = M.Snapshot.show x in
  Alcotest.(check string) "build with encoded Interpreter.args" expected actual

let test_decode_marshal () =
  let val0 = 0 in
  let val1 = 1 in
  let arg0 = M.Encode.to_string val0 [] in
  let arg1 = M.Encode.to_string val1 [] in
  let x = M.Snapshot.build "foo" [ [ arg0; arg1 ] ] in
  let y = M.Encode.to_string x [] in

  let b =
    val0 = M.Encode.from_string arg0
    && val1 = M.Encode.from_string arg1
    && x = M.Encode.from_string y
  in
  Alcotest.(check bool) "decode basic values" true b

let test_decode_args () =
  let open Osnap__Interpreter in
  let x = Cons (0, Cons ("0", Nil)) in
  let y = M.Encode.to_string x [] in

  let b = x = M.Encode.from_string y in

  Alcotest.(check bool) "decode args" true b

let test_decode_applications () =
  let open Osnap__Spec in
  let spec = int ^> int ^>> string_of_int in
  let f = M.Encode.to_string in
  let snapshot =
    M.Snapshot.build "add" [ [ f 0 []; f 1 []; f 2 [] ] ]
    |> M.Snapshot.decode spec
  in

  let expected = {|{ name = "add"; applications = [["0"; "1"; "2"]] }|} in
  let actual = M.Snapshot.show snapshot in

  Alcotest.(check string) "decode applications inside snapshot" expected actual

let test_read_none () =
  let b = M.Snapshot.read "" |> Option.is_none in
  Alcotest.(check bool) "read None = None" true b

let test_write_read () =
  let f = M.Encode.to_string in
  let snapshot = M.Snapshot.build "add" [ [ f 0 []; f 1 []; f 2 [] ] ] in
  let path = "./add.osnap" in
  let () = M.Snapshot.write path snapshot in
  let actual = Option.get @@ M.Snapshot.read path in

  let b = actual = snapshot in
  Alcotest.(check bool) "test write read" true b

let tests =
  ( "Memory",
    Alcotest.
      [
        test_case "test encode empty" `Quick test_encode_empty;
        test_case "test encode nonempty" `Quick test_encode_nonempty;
        test_case "test encode with marshal" `Quick test_encode_marshal;
        test_case "test encode with marshal args" `Quick test_encode_args;
        test_case "test decode with marshal" `Quick test_decode_marshal;
        test_case "test decode with marshal args" `Quick test_decode_args;
        test_case "test read none" `Quick test_read_none;
        test_case "test decode applications" `Quick test_decode_applications;
        test_case "test write read" `Quick test_write_read;
      ] )
