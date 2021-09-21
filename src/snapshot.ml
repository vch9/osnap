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
