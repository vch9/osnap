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
  }

  type t = Test : ('a, 'b, 'c) cell -> t

  let path (Test { path; _ }) = path

  let make ?(count = 10) ~path ~spec ~name f =
    Test { path; spec; f; count; name }
end

module Snapshot = struct
  open Test
  module M = Memory

  let show snapshot =
    let name = M.Snapshot.name snapshot in
    let applications = M.Snapshot.applications snapshot in

    let f n l =
      let rec aux = function
        | [ x ] -> "= " ^ x
        | x :: xs -> x ^ " " ^ aux xs
        | [] -> assert false
      in
      n ^ " " ^ aux l
    in
    List.fold_left (fun acc x -> acc ^ f name x ^ "\n") "" applications

  let rec encode_applications : type a b. (a, b) Interpreter.args -> string list
      = function
    | Cons (x, xs) -> M.Encode.to_string x [] :: encode_applications xs
    | _ -> []

  let make ?rand (Test { spec; f; count; name; _ }) =
    let spec_to_args =
      Option.fold
        ~none:Interpreter.spec_to_args (* For testing only *)
        ~some:(fun rand -> Interpreter.Internal_for_tests.spec_to_args rand)
        rand
    in
    let applications =
      List.init count (fun _ ->
          let args = spec_to_args spec in
          let res = Interpreter.(args_to_expr (Fun f) args |> interpret) in
          encode_applications args @ [ M.Encode.to_string res [] ])
    in
    M.Snapshot.build name applications
end

module Runner = struct
  type mode = Interactive | Promote | Error

  let run _mode test =
    let _prev = Memory.Snapshot.read (Test.path test) in
    failwith "TODO"

  let run_tests ?(mode = Error) tests =
    let () = List.iter (run mode) tests in
    0
end
