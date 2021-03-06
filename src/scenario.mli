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

(** This module handles internal representation of function. We use
    module {!Spec} to generate type {!args}.
    We then use {!args} to create a {!t}, stored in-memory as the
    regression tests. *)

type 'r res = ('r, string) result

type ('fn, 'r) t =
  | Res : 'r res -> ('r, 'r) t
  | Cons : 'a * ('fn, 'r) t -> ('a -> 'fn, 'r) t

(** [spec_to_scenario spec f] instantiate arguments values using generators inside [spec],
    applies them to [f] *)
val spec_to_scenario :
  rand:Random.State.t -> ('fn, 'r) Spec.t -> 'fn -> ('fn, 'r) t

(** [encoding_scenario spec] encodes a scenario using [spec], every element inside
    [spec] __must__ contains an encoding *)
val encoding_scenario : ('fn, 'r) Spec.t -> ('fn, 'r) t Data_encoding.encoding

val pp : Format.formatter -> ('fn, 'r) Spec.t -> ('fn, 'r) t -> unit

(** [reapply scenario f] takes generated parameters inside [scenario] and
    apply them to [f] *)
val reapply : ('fn, 'r) t -> 'fn -> ('fn, 'r) t
