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

val pp : Format.formatter -> ('fn, 'r) Spec.t -> ('fn, 'r) t -> unit

val to_string : ('fn, 'r) Spec.t -> ('fn, 'r) t -> string

val encoding : ('fn, 'r) Spec.t -> ('fn, 'r) t Data_encoding.encoding

(** [create ~name ~spec ~f n] creates a snapshot with [n] scenarios
    built using [spec] and [f] *)
val create :
  ?rand:Random.State.t ->
  name:string ->
  spec:('fn, 'r) Spec.t ->
  f:'fn ->
  int ->
  ('fn, 'r) t

(** [create_from_snapshot snapshot f] applies [f] on each scenario inside
    the [snapshot] *)
val create_from_snapshot : ('fn, 'r) t -> 'fn -> ('fn, 'r) t

(** [encode ?spec ~mode ~path snapshot] encodes the [snapshot] and write
    the encoded version in [path]

    Two possible modes of encoding:
    - `Binary: the snapshot is encoded in binary using the library Marshal
    - `Encoding: the snapshot is encoded in a JSON format using the Data_encoding
    library. /!\ [spec] must be present in that case, and every field [encoding] should
    be present inside [spec]. *)
val encode :
  ?spec:('fn, 'r) Spec.t ->
  mode:[< `Binary | `Encoding ] ->
  path:string ->
  ('fn, 'r) t ->
  unit

(** [decode ?spec ~mode ~path ()] decodes the [snapshot] and read the
    encoded version in [path]

    Two possible modes of encoding:
    - `Binary: the snapshot is encoded in binary using the library Marshal
    - `Encoding: the snapshot is encoded in a JSON format using the Data_encoding
    library. /!\ [spec] must be present in that case, and every field [encoding] should
    be present inside [spec].

    /!\ The mode of decoding must be the same used for encoding /!\ *)
val decode :
  ?spec:('fn, 'r) Spec.t ->
  mode:[< `Binary | `Encoding ] ->
  path:string ->
  unit ->
  ('fn, 'r) t
