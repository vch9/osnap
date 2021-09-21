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

open Spec
module Gen = QCheck.Gen

type ('fn, 'r) t =
  | Res : 'r -> ('r, 'r) t
  | Cons : 'a * ('fn, 'r) t -> ('a -> 'fn, 'r) t

let rec spec_to_scenario :
    type fn r. ?rand:Random.State.t -> (fn, r) Spec.t -> fn -> (fn, r) t =
 fun ?rand spec f ->
  match spec with
  | Arrow ({ gen; _ }, spec) ->
      let x = Gen.generate1 ?rand gen in
      Cons (x, spec_to_scenario ?rand spec (f x))
  | Result _ -> Res f

let rec encoding_scenario :
    type fn r. (fn, r) Spec.t -> (fn, r) t Data_encoding.encoding =
 fun spec ->
  let open Spec in
  let open Data_encoding in
  match spec with
  | Result { encoding; _ } ->
      assert (Option.is_some encoding) ;
      let encoding = Option.get encoding in
      conv (function Res x -> x | _ -> assert false) (fun x -> Res x) encoding
  | Arrow ({ encoding; _ }, spec) ->
      assert (Option.is_some encoding) ;
      let encoding = Option.get encoding in
      conv
        (function Cons (a, b) -> (a, b) | _ -> assert false)
        (fun (a, b) -> Cons (a, b))
        (tup2 encoding @@ encoding_scenario spec)

let rec to_string : type fn r. (fn, r) Spec.t -> (fn, r) t -> string =
 fun spec scenario ->
  match (spec, scenario) with
  | (Result { printer; _ }, Res r) -> Format.sprintf "-> %s@." (printer r)
  | (Arrow ({ printer; _ }, spec), Cons (fn, scenario)) ->
      let printer = Spec.default_printer printer in
      Format.sprintf "%s -> %s" (printer fn) (to_string spec scenario)
  | _ -> assert false

let pp fmt spec scenario = Format.fprintf fmt "%s" (to_string spec scenario)
