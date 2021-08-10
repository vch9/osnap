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

module Test = struct
  type ('a, 'b, 'c) cell = {
    path : string;
    name : string;
    spec : ('a -> 'b, 'c) Spec.t;
    f : 'a -> 'b;
    count : int;
    rand : Random.State.t option;
  }

  type t = Test : ('a, 'b, 'c) cell -> t

  let make ?(count = 10) ?rand ~path ~spec ~name f =
    Test { path; spec; f; count; name; rand }
end

module Snapshot = struct
  open Test
  module M = Memory

  let show snapshot =
    let name = M.Snapshot.name snapshot in
    let applications = M.Snapshot.applications snapshot in

    List.fold_left
      (fun acc (args, res) -> Printf.sprintf "%s %s %s\n%s" name args res acc)
      ""
      applications

  let make ?rand (Test { spec; f; count; name; rand = rand'; _ }) =
    let rand = match (rand, rand') with (None, x) -> x | (x, _) -> x in
    let spec_to_args =
      Option.fold
        ~none:Interpreter.spec_to_args
        ~some:(fun rand -> Interpreter.Internal_for_tests.spec_to_args rand)
        (* For testing only *)
        rand
    in
    let applications =
      List.init count (fun _ ->
          let args = spec_to_args spec in
          let res = Interpreter.(args_to_expr (Fun f) args |> interpret) in
          (M.Encode.to_string args [], M.Encode.to_string res []))
    in
    M.Snapshot.build name applications

  let next test snapshot =
    let Test.(Test { f; name; _ }) = test in
    match snapshot with
    | None -> make test
    | Some prev ->
        let decoded_applications =
          M.Snapshot.applications prev
          |> List.map (fun (x, y) ->
                 (M.Encode.from_string x, M.Encode.from_string y))
        in
        let new_applications =
          List.map
            (fun (args, _) ->
              let res = Interpreter.(args_to_expr (Fun f) args |> interpret) in
              (M.Encode.to_string args [], M.Encode.to_string res []))
            decoded_applications
        in
        M.Snapshot.build name new_applications
end

module Runner = struct
  type mode = Interactive | Promote | Error

  let error diff =
    match diff with
    | Diff.Same -> ()
    | Diff.(New s) ->
        let msg = Printf.sprintf "Error: no previous snapshot, new:\n%s" s in
        failwith msg
    | Diff.(Diff s) ->
        let msg =
          Printf.sprintf "Error: difference between old and new snapshot:\n%s" s
        in
        failwith msg

  let run mode test =
    let Test.(Test { spec; path; _ }) = test in
    let prev = Memory.Snapshot.read path in
    let prev_str =
      Option.fold
        ~none:None
        ~some:(fun x ->
          Option.some @@ Snapshot.show @@ Memory.Snapshot.decode_str spec x)
        prev
    in
    let next = Snapshot.next test prev |> Memory.Snapshot.decode_str spec in
    let next_str = Snapshot.show next in

    let diff = Diff.diff prev_str next_str in
    match mode with
    | Error -> error diff
    | Promote -> failwith "todo"
    | Interactive -> failwith "todo"

  let run_tests ?(mode = Error) tests =
    let () = List.iter (run mode) tests in
    0
end
