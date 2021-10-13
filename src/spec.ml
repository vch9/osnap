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

let default_printer printer =
  Option.value ~default:(fun _ -> "<opaque>") printer

let printer_list f l =
  let rec printer_elements = function
    | [] -> ""
    | [ x ] -> f x
    | x :: xs -> Printf.sprintf "%s , %s" (f x) (printer_elements xs)
  in
  Printf.sprintf "[%s]" (printer_elements l)

let printer_list f l = printer_list (default_printer f) l

let printer_array f l = printer_list f @@ Array.to_list l

let printer_option f x =
  let f = default_printer f in
  match x with Some x -> Printf.sprintf "Some (%s)" (f x) | None -> "None"

let encoding_list = function
  | None -> None
  | Some x -> Option.some @@ Data_encoding.list x

let encoding_array = function
  | None -> None
  | Some x -> Option.some @@ Data_encoding.array x

let encoding_option = function
  | None -> None
  | Some x -> Option.some @@ Data_encoding.option x

module Result = struct
  type 'a t = { printer : 'a printer; encoding : 'a encoding option }

  let build ?encoding printer : 'a t = { printer; encoding }

  let unit =
    let printer = Unit.to_string in
    let encoding = Data_encoding.unit in
    build ~encoding printer

  let bool =
    let printer = Bool.to_string in
    let encoding = Data_encoding.bool in
    build printer ~encoding

  let int =
    let printer = Int.to_string in
    let encoding = Data_encoding.int31 in
    build ~encoding printer

  let float =
    let printer = Float.to_string in
    let encoding = Data_encoding.float in
    build ~encoding printer

  let char =
    let printer = Format.sprintf "%c" in
    (* let encoding = Data_encoding.char in *)
    build printer

  let string =
    let printer x = x in
    let encoding = Data_encoding.string in
    build ~encoding printer

  let list t =
    let printer = printer_list @@ Option.some t.printer in
    let encoding = encoding_list t.encoding in
    build ?encoding printer

  let array t =
    let printer = printer_array @@ Option.some t.printer in
    let encoding = encoding_array t.encoding in
    build ?encoding printer

  let option t =
    let printer = printer_option @@ Option.some t.printer in
    let encoding = encoding_option t.encoding in
    build ?encoding printer
end

type ('fn, 'r) t =
  | Result : 'r Result.t -> ('r, 'r) t
  | Arrow : 'a spec * ('fn, 'r) t -> ('a -> 'fn, 'r) t

let rec can_encode : type fn r. (fn, r) t -> bool =
 fun spec ->
  match spec with
  | Result { encoding; _ } -> Option.is_some encoding
  | Arrow ({ encoding; _ }, spec) -> Option.is_some encoding && can_encode spec

let of_gen gen = { gen; printer = None; encoding = None }

let build ?printer ?encoding gen = { gen; printer; encoding }

let unit =
  let gen = Gen.unit in
  build ~printer:Result.unit.printer ?encoding:Result.unit.encoding gen

let bool =
  let gen = Gen.bool in
  build ~printer:Result.bool.printer ?encoding:Result.bool.encoding gen

let int =
  let gen = Gen.(-1073741823 -- 1073741823) in
  build ~printer:Result.int.printer ?encoding:Result.int.encoding gen

let float =
  let gen = Gen.float in
  build ~printer:Result.float.printer ?encoding:Result.float.encoding gen

let char =
  let gen = Gen.char in
  build ~printer:Result.char.printer ?encoding:Result.char.encoding gen

let string =
  let gen = Gen.string in
  build ~printer:Result.string.printer ?encoding:Result.string.encoding gen

let list spec =
  let printer =
    match spec.printer with
    | Some _ -> Option.some @@ printer_list spec.printer
    | None -> None
  in
  let encoding = encoding_list spec.encoding in
  let gen = Gen.list spec.gen in
  build ?encoding ?printer gen

let array spec =
  let printer =
    match spec.printer with
    | Some _ -> Option.some @@ printer_array spec.printer
    | None -> None
  in
  let encoding = encoding_array spec.encoding in
  let gen = Gen.array spec.gen in
  build ?encoding ?printer gen

let option spec =
  let printer =
    match spec.printer with
    | Some _ -> Option.some @@ printer_option spec.printer
    | None -> None
  in
  let encoding = encoding_option spec.encoding in
  let gen = Gen.opt spec.gen in
  build ?encoding ?printer gen

let ( ^> ) x y = Arrow (x, y)

let ( ^>> ) x y = Arrow (x, Result y)
