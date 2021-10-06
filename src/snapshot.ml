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

type ('fn, 'r) t =
  | Snapshot : {
      name : string;
      scenarios : ('fn, 'r) Scenario.t list;
    }
      -> ('fn, 'r) t

type mode = Marshal | Data_encoding

let pp_scenarios spec fmt scenarios =
  let pp fmt scenario =
    Format.pp_print_char fmt '\t' ;
    Scenario.pp fmt spec scenario
  in

  Format.pp_print_list ~pp_sep:Format.pp_print_newline pp fmt scenarios

let pp fmt spec (Snapshot { name; scenarios }) =
  Format.fprintf
    fmt
    "{@.  name = %s;@.  scenarios = [@.@[<hov 2>%a@]@.  ]@.}"
    name
    (pp_scenarios spec)
    scenarios

let to_string spec snapshot =
  let pp fmt = pp fmt spec in
  Format.asprintf "%a" pp snapshot

let encoding : type fn r. (fn, r) Spec.t -> (fn, r) t Data_encoding.encoding =
 fun spec ->
  let open Data_encoding in
  conv
    (fun (Snapshot { name; scenarios }) -> (name, scenarios))
    (fun (name, scenarios) -> Snapshot { name; scenarios })
    (obj2
       (req "name" string)
       (req "scenarios" @@ list @@ Scenario.encoding_scenario spec))

let create ~rand ~name ~spec ~f n =
  let scenarios =
    List.init n (fun _ -> Scenario.spec_to_scenario ~rand spec f)
  in
  Snapshot { name; scenarios }

let create_from_snapshot (Snapshot { name; scenarios }) f =
  let scenarios =
    List.map (fun scenario -> Scenario.reapply scenario f) scenarios
  in
  Snapshot { name; scenarios }

let encode ?spec ~mode ~path snapshot =
  match mode with
  | Marshal ->
      let oc = open_out path in
      let () = Marshal.to_channel oc snapshot [] in
      close_out oc
  | Data_encoding ->
      if Option.is_none spec then
        raise
          (Invalid_argument "Cannot encode a snapshot without the specification")
      else
        let spec = Option.get spec in
        if Spec.can_encode spec then
          let json = Data_encoding.Json.construct (encoding spec) snapshot in
          let oc = open_out path in
          let () =
            Printf.fprintf oc "%s" @@ Data_encoding.Json.to_string json
          in
          close_out oc
        else
          raise
            (Invalid_argument
               "Some encoding fields in the specification are missing")

exception SnapshotNotFound of string

exception DataEncodingError of string

exception DataEncodingMissing

exception MarshalError of string

let decode ?spec ~mode ~path () =
  if Sys.file_exists path then
    match mode with
    | Marshal -> (
        try
          let ic = open_in path in
          let x = Marshal.from_channel ic in
          let () = close_in ic in
          x
        with Failure s -> raise (MarshalError s))
    | Data_encoding -> (
        if Option.is_none spec then
          raise
            (Invalid_argument
               "Cannot decode a snapshot without the specification")
        else
          let spec = Option.get spec in
          let () =
            if not (Spec.can_encode spec) then raise DataEncodingMissing
          in
          let json = Common.read_file path |> Data_encoding.Json.from_string in
          let json =
            match json with
            | Ok x -> x
            | Error er -> raise (DataEncodingError er)
          in
          try Data_encoding.Json.destruct (encoding spec) json
          with e -> raise (DataEncodingError (Printexc.to_string e)))
  else raise (SnapshotNotFound (path ^ " does not exists"))

let decode_opt ?spec ~mode ~path () =
  try decode ?spec ~mode ~path () |> Option.some with
  | SnapshotNotFound _ -> None
  | DataEncodingError s ->
      let () =
        Printf.printf
          "Error: snapshot at %s could not be decoded using Data_encoding:\n\
           \t%s"
          path
          s
      in
      exit 1
  | MarshalError s ->
      let () =
        Printf.printf
          "Error: snapshot at %s could not be decoded using Marshal:\n\t%s"
          path
          s
      in
      exit 1
  | DataEncodingMissing ->
      let () =
        Printf.printf "Error: encoding are missing in the specification"
      in
      exit 1
