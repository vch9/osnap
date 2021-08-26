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

module Gen = QCheck.Gen

type 'a gen = 'a Gen.t

type 'a printer = 'a -> string

type 'a encoding = 'a Data_encoding.t

type 'a spec = {
  gen : 'a gen;
  printer : 'a printer option;
  encoding : 'a encoding option;
}

type ('fn, 'r) t =
  | Result : 'a printer -> ('a, 'a) t
  | Arrow : 'a spec * ('fn, 'r) t -> ('a -> 'fn, 'r) t

let default_printer printer =
  Option.value ~default:(fun _ -> "<opaque>") printer

let of_gen gen = { gen; printer = None; encoding = None }

let build ?printer ?encoding gen = { gen; printer; encoding }

let unit =
  let gen = Gen.unit in
  let printer = Unit.to_string in
  let encoding = Data_encoding.unit in
  build ~printer ~encoding gen

let bool =
  let gen = Gen.bool in
  let printer = Bool.to_string in
  let encoding = Data_encoding.bool in
  build ~printer ~encoding gen

let int =
  let gen = Gen.int in
  let printer = Int.to_string in
  let encoding = Data_encoding.int31 in
  build ~printer ~encoding gen

let float =
  let gen = Gen.float in
  let printer = Float.to_string in
  let encoding = Data_encoding.float in
  build ~printer ~encoding gen

let char =
  let gen = Gen.char in
  let printer = Format.sprintf "%c" in
  (* let encoding = Data_encoding.char in *)
  build ~printer gen

let string =
  let gen = Gen.string in
  let printer x = x in
  let encoding = Data_encoding.string in
  build ~printer ~encoding gen

let option spec =
  let printer x =
    let f = default_printer spec.printer in
    match x with Some x -> Printf.sprintf "Some (%s)" (f x) | None -> "None"
  in
  let gen = Gen.opt spec.gen in
  let printer = printer in

  let encoding =
    Option.fold
      ~none:None
      ~some:(fun x -> Option.some @@ Data_encoding.option x)
      spec.encoding
  in

  build ~printer ?encoding gen

let printer_list f l =
  let rec printer_elements = function
    | [] -> ""
    | [ x ] -> f x
    | x :: xs -> Printf.sprintf "%s , %s" (f x) (printer_elements xs)
  in
  Printf.sprintf "[%s]" (printer_elements l)

let array spec =
  let printer x =
    Array.to_list x |> printer_list (default_printer spec.printer)
  in
  let gen = Gen.array spec.gen in
  let encoding =
    Option.fold
      ~none:None
      ~some:(fun x -> Option.some @@ Data_encoding.array x)
      spec.encoding
  in
  build ~printer ?encoding gen

let list spec =
  let printer = printer_list (default_printer spec.printer) in
  let gen = Gen.list spec.gen in
  let encoding =
    Option.fold
      ~none:None
      ~some:(fun x -> Option.some @@ Data_encoding.list x)
      spec.encoding
  in
  build ~printer ?encoding gen

let ( ^> ) x y = Arrow (x, y)

let ( ^>> ) x y = Arrow (x, Result y)
