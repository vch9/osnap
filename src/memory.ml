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

(** [create_osnap path] creates [.osnap/.keep] directory at [path] *)
let create_osnap path =
  let path = Printf.sprintf "%s.osnap/" path in
  let () = Unix.mkdir path 0o777 in
  let file = path ^ ".keep" in
  let oc = open_out file in
  close_out oc

(** [read_osnap path] reads content at [path] *)
let read_osnap path =
  let lines = ref [] in
  let ic = open_in path in
  try
    while true do
      lines := input_line ic :: !lines
    done ;
    !lines |> String.concat "\n"
  with End_of_file ->
    close_in ic ;
    List.rev !lines |> String.concat "\n"

(** [write_osnap path content] writes [content] at [path] *)
let write_osnap path content =
  let oc = open_out path in
  let () = Printf.fprintf oc "%s" content in
  close_out oc
