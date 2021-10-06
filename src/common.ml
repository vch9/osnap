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

let dir = ".osnap"

let sep = Filename.dir_sep

let fname name = dir ^ sep ^ name

let opt_path name =
  let cwd = Sys.getcwd () in
  let lcwd = Str.split (Str.regexp "/_build/default/") cwd in

  let name = fname name in
  match lcwd with
  | [ pwd; path ] -> pwd ^ sep ^ path ^ sep ^ name
  | _ -> cwd ^ sep ^ name

let full_path path =
  if Filename.is_relative path then Sys.getcwd () ^ sep ^ path else path

let read_file path =
  let lines = ref [] in
  let ic = open_in path in
  try
    while true do
      lines := input_line ic :: !lines
    done ;
    assert false
  with End_of_file ->
    close_in ic ;
    List.rev !lines |> String.concat ""

let write path s =
  let dir = Filename.dirname path in
  if (not (Sys.file_exists path)) && not (Sys.file_exists dir) then
    Unix.mkdir dir 0o755 ;
  let oc = open_out path in
  let () = output_string oc s in
  close_out oc
