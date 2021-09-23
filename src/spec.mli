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

(** Spec takes inspiration from OCaml's property-based testing library Monolith.
    The idea is that the programmer describes the function specification through
    the set of generators from QCheck and a printer.

    Examples:

    - Addition specification:

    {[
      let add = (+)
      let spec_add = Spec.(int ^> int ^>> string_of_int)
    ]}

    - List sum specification:

    {[
      let sum = List.fold_left ( + )
      let spec_sum = list int ^>> string_of_int
    ]}
*)

(** ['a gen] is used to generate random values inside {!spec}.
    QCheck combinators are available using [Spec.Gen]. *)
type 'a gen = 'a QCheck.Gen.t

(** ['a printer] is used to store randomly generated values *)
type 'a printer = 'a -> string

(** ['a encoding] is used to encode values in memory *)
type 'a encoding = 'a Data_encoding.t

(** ['a spec] combines an ['a gen] and a printer *)
type 'a spec = {
  gen : 'a gen;
  printer : 'a printer option;
  encoding : 'a Data_encoding.t option;
}

type 'a result = { printer : 'a printer; encoding : 'a Data_encoding.t option }

(** [t] is the specification type, describing a function.
    Thus [t] declaration must end with {! (^>>) }. *)
type ('fn, 'r) t =
  | Result : 'r result -> ('r, 'r) t
  | Arrow : 'a spec * ('fn, 'r) t -> ('a -> 'fn, 'r) t

(** [default_printer printer] creates a default printer if [printer] is absent *)
val default_printer : ('a -> string) option -> 'a -> string

(** [build ?printer ?encoding gen] builds an ['a spec] with optional fields *)
val build : ?printer:'a printer -> ?encoding:'a encoding -> 'a gen -> 'a spec

(** [build_result ?encoding printer] builds an ['a result] with optionally an encoding *)
val build_result : ?encoding:'a encoding -> 'a printer -> 'a result

(** [of_gen gen] creates an ['a spec] with no printer *)
val of_gen : 'a gen -> 'a spec

(** [unit] specification *)
val unit : unit spec

(** [bool] specification *)
val bool : bool spec

(** [float] specification *)
val float : float spec

(** [int] specification *)
val int : int spec

(** [char] specification *)
val char : char spec

(** [string] specification *)
val string : string spec

(** [option spec] creates an option spec for [spec] *)
val option : 'a spec -> 'a option spec

(** [array spec] creates an array spec for [spec] *)
val array : 'a spec -> 'a array spec

(** [list spec] creates a list spec for [spec] *)
val list : 'a spec -> 'a list spec

(** [(^>) x y] combines spec [x] and [y] to create [x -> y] *)
val ( ^> ) : 'a spec -> ('b, 'c) t -> ('a -> 'b, 'c) t

(** [(^>>) x res] combines a specification and printer for the result type *)
val ( ^>> ) : 'a spec -> 'b result -> ('a -> 'b, 'b) t
