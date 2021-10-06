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

open Test_helpers
module Test = Osnap.Test
module Runner = Osnap.Runner
module Snapshot = Osnap__Snapshot

let eq_snapshot = Test_snapshot.eq

let eq_res : Runner.res -> Runner.res -> bool =
  let eq_aux x y =
    match (x, y) with
    | (`Passed x, `Passed y) -> x = y
    | (`Promoted x, `Promoted y) -> x = y
    | (`Ignored x, `Ignored y) -> x = y
    | (`Error (x1, y1), `Error (x2, y2)) -> x1 = x2 && y1 = y2
    | _ -> false
  in
  List.equal eq_aux

let pp_res =
  let pp_aux fmt = function
    | `Passed s -> Format.fprintf fmt "`Passed %s" s
    | `Promoted s -> Format.fprintf fmt "`Promoted %s" s
    | `Ignored s -> Format.fprintf fmt "`Ignored %s" s
    | `Error (s1, s2) -> Format.fprintf fmt "`Error (%s, %s)" s1 s2
  in
  Format.pp_print_list pp_aux

let check_res = Alcotest.of_pp pp_res

let path = "./foo"

and name = "foo"

and spec = spec_add

and count = 5

let test_add seed = Test.make ~count ~rand:(RS.make seed) ~spec ~name ~path add

let snapshot_add seed =
  Snapshot.create ~rand:(RS.make seed) ~name ~spec ~f:add count

let test_mul seed =
  Test.make ~count ~rand:(RS.make seed) ~spec ~name ~path Int.mul

let snapshot_mul seed =
  Snapshot.create ~rand:(RS.make seed) ~name ~spec ~f:Int.mul count

let test_promote_data_encoding_good () =
  let () = Sys.remove path in
  let _ =
    Runner.run_tests ~encoding:Data_encoding ~mode:Promote [ test_add seed ]
  in
  let actual = Snapshot.decode ~mode:Data_encoding ~spec:spec_add ~path () in
  Alcotest.(check (eq_snapshot spec_add))
    "promote Data_encoding s; decode s = s"
    (snapshot_add seed)
    actual

let test_promote_marshal_good () =
  let () = Sys.remove path in
  let _ = Runner.run_tests ~encoding:Marshal ~mode:Promote [ test_add seed ] in
  let actual = Snapshot.decode ~mode:Marshal ~spec:spec_add ~path () in
  Alcotest.(check (eq_snapshot spec_add))
    "promote Marshal s; decode s = s"
    (snapshot_add seed)
    actual

(* Re-running the same test should not change the snapshot *)
let test_promote_twice () =
  let () = Sys.remove path in
  let _ =
    Runner.run_tests
      ~encoding:Marshal
      ~mode:Promote
      [ test_add seed; test_add [| 0 |] ]
  in
  let expected = snapshot_add seed in
  let actual = Snapshot.decode ~mode:Marshal ~spec:spec_add ~path () in
  Alcotest.(check (eq_snapshot spec_add))
    "promote the same test twice gives the same snapshot"
    expected
    actual

(* Promote from a snapshot with the addition to a test with the multiplication should
   produce the same snapshot as {!snapshot_mul} *)
let test_promote_changes_snapshot () =
  let () = Sys.remove path in
  let _ =
    Runner.run_tests
      ~encoding:Marshal
      ~mode:Promote
      [ test_add seed; test_mul [| 0 |] ]
  in
  let expected = snapshot_mul seed in
  let actual = Snapshot.decode ~mode:Marshal ~spec:spec_add ~path () in
  Alcotest.(check (eq_snapshot spec_add))
    "promote from add to mul changes the snapshot"
    expected
    actual

(* Run without precedent snapshot on mode Error raises an error *)
let test_error_no_snapshot () =
  let () = Sys.remove path in
  let expected =
    [ `Error ("foo", Printf.sprintf "Error: no previous snapshot at %s" path) ]
  in
  let actual =
    Runner.run_tests_with_res
      Marshal
      Error
      Format.std_formatter
      [ test_add seed ]
    |> fst
  in
  Alcotest.check check_res "should returns errors" expected actual

(* Run Promote then Error the same test passes *)
let test_promote_error () =
  let expected = [ `Passed "foo" ] in
  let _ =
    Runner.run_tests_with_res
      Marshal
      Promote
      Format.std_formatter
      [ test_add seed ]
  in
  let actual =
    Runner.run_tests_with_res
      Marshal
      Error
      Format.std_formatter
      [ test_add seed ]
    |> fst
  in
  Alcotest.check check_res "should pass" expected actual

(* Run Promote then Error with a different scenarios *)
let test_error_with_diff () =
  let expected = 1 in
  let _ =
    Runner.run_tests_with_res
      Marshal
      Promote
      Format.std_formatter
      [ test_add seed ]
  in
  let actual =
    Runner.run_tests_with_res
      Marshal
      Error
      Format.std_formatter
      [ test_mul seed ]
    |> snd
  in
  Alcotest.(check int) "should fail" expected actual

let gen_mode_args =
  let open QCheck.Gen in
  let args = [ "--mode"; "-m" ] |> List.map pure in
  let values =
    [ "error"; "interactive"; "promote" ] |> List.map pure |> fun correct ->
    frequency [ (5, oneof correct); (1, string_readable) ]
  in
  pair (oneof args) values >>= fun (arg, value) -> pure (arg, value)

let gen_encoding_args =
  let open QCheck.Gen in
  let args = [ "--color"; "-c" ] |> List.map pure in
  let values =
    [ "true"; "false" ] |> List.map pure |> fun correct ->
    frequency [ (5, oneof correct); (1, string_readable) ]
  in
  pair (oneof args) values >>= fun (arg, b) -> pure (arg, b)

let gen_color_args =
  let open QCheck.Gen in
  let args = [ "--encoding"; "-e" ] |> List.map pure in
  let values =
    [ "marshal"; "data_encoding" ] |> List.map pure |> fun correct ->
    frequency [ (5, oneof correct); (1, string_readable) ]
  in
  pair (oneof args) values >>= fun (arg, value) -> pure (arg, value)

let arb_mode_args = QCheck.make gen_mode_args

let arb_encoding_args = QCheck.make gen_encoding_args

let arb_color_args = QCheck.make gen_color_args

let test_parse_mode =
  QCheck.Test.make ~name:"CLI parse mode" arb_mode_args (fun (arg, mode) ->
      let argv = [| arg; mode |] in
      let args = Osnap.Runner.Cli.parse argv in

      (mode = "error" && args.mode = Error)
      || (mode = "promote" && args.mode = Promote)
      || (mode = "interactive" && args.mode = Interactive)
      || args.mode = Error)

let test_parse_color =
  QCheck.Test.make ~name:"CLI parse color" arb_color_args (fun (arg, color) ->
      let argv = [| arg; color |] in
      let args = Osnap.Runner.Cli.parse argv in

      (color = "true" && args.color)
      || (color = "false" && not args.color)
      || not args.color)

let test_parse_encoding =
  QCheck.Test.make
    ~name:"CLI parse encoding"
    arb_encoding_args
    (fun (arg, encoding) ->
      let argv = [| arg; encoding |] in
      let args = Osnap.Runner.Cli.parse argv in

      (encoding = "marshal" && args.encoding = Marshal)
      || (encoding = "data_encoding" && args.encoding = Data_encoding)
      || args.encoding = Data_encoding)

let qcheck_tests =
  [ test_parse_mode; test_parse_color ]
  |> List.map (QCheck_alcotest.to_alcotest ~rand)

let tests =
  Alcotest.
    [
      test_case
        "test_promote_data_encoding_good"
        `Quick
        test_promote_data_encoding_good;
      test_case "test_promote_marshal_good" `Quick test_promote_marshal_good;
      test_case "test_promote_twice" `Quick test_promote_twice;
      test_case
        "test_promote_changes_snapshot"
        `Quick
        test_promote_changes_snapshot;
      test_case "test_error_no_snapshot" `Quick test_error_no_snapshot;
      test_case "test_promote_error" `Quick test_promote_error;
      test_case "test_error_with_diff" `Quick test_error_with_diff;
    ]

let tests = ("Osnap", qcheck_tests @ tests)
