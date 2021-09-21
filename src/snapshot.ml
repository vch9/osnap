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

let pp fmt spec (Snapshot { name; scenarios }) =
  let pp_aux fmt = Scenario.pp fmt spec in
  let pp_list fmt scenarios = Format.pp_print_list pp_aux fmt scenarios in
  Format.fprintf
    fmt
    "{@.name = %s;@.scenarios = @[<hov 2>%a@]@.}"
    name
    pp_list
    scenarios

let encoding : type fn r. (fn, r) Spec.t -> (fn, r) t Data_encoding.encoding =
 fun spec ->
  let open Data_encoding in
  conv
    (fun (Snapshot { name; scenarios }) -> (name, scenarios))
    (fun (name, scenarios) -> Snapshot { name; scenarios })
    (obj2
       (req "name" string)
       (req "scenarios" @@ list @@ Scenario.encoding_scenario spec))

let create ?rand ~name ~spec ~f n =
  let scenarios =
    List.init n (fun _ -> Scenario.spec_to_scenario ?rand spec f)
  in
  Snapshot { name; scenarios }

let encode ?spec ~mode ~path snapshot =
  match mode with
  | `Binary ->
      let oc = open_out path in
      let () = Marshal.to_channel oc snapshot [] in
      close_out oc
  | `Encoding ->
      if Option.is_none spec then
        raise
          (Invalid_argument "Cannot encode a snapshot without the specification")
      else
        let spec = Option.get spec in
        let json = Data_encoding.Json.construct (encoding spec) snapshot in
        let oc = open_out path in
        let () = Printf.fprintf oc "%s" @@ Data_encoding.Json.to_string json in
        close_out oc

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

let decode ?spec ~mode ~path () =
  if Sys.file_exists path then
    match mode with
    | `Binary ->
        let ic = open_in path in
        let x = Marshal.from_channel ic in
        let () = close_in ic in
        x
    | `Encoding ->
        if Option.is_none spec then
          raise
            (Invalid_argument
               "Cannot decode a snapshot without the specification")
        else
          let spec = Option.get spec in
          let json = read_file path |> Data_encoding.Json.from_string in
          let json = match json with Ok x -> x | Error _ -> failwith "todo" in
          Data_encoding.Json.destruct (encoding spec) json
  else raise (Invalid_argument (path ^ " does not exists"))
