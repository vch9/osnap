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

(** {1 OCaml random snapshot testing} *)

(** The library takes inspiration from ppx_expect, we add random generators
    to store random application of functions. A snapshot will be the random
    applications on a function with its, the snapshot will be latter use to
    diff between old and new version of a function.

    - {!Test} is used to describe a single test, that is, where the snapshot
      will be stored and the function under test with its specification.

    Example:

    - Test addition:

    {[
      let test =
        let spec = OSnap.Spec.(int ^> int ^>> int) in
        Osnap.Test.make ~path:".osnap/add" ~spec (+)
    ]}
    

    - {!Runner} is used to run tests

    @see <https://github.com/janestreet/ppx_expect>
*)

(** Spec takes inspiration from OCaml's property-based testing library Monolith.
    The idea is that the programmer describes the function specification through
    the set of generators from QCheck and a printer.

    Examples:

    - Addition specification:

    {[
      let add = (+)
      let spec_add = Spec.(int ^> int ^>> int)
    ]}

    - List sum specification:

    {[
      let sum = List.fold_left ( + )
      let spec_sum = int ^>> int
    ]}
*)
module Spec : sig
  module Gen : sig
    include module type of QCheck.Gen
  end

  (** ['a gen] is used to generate random values inside {!spec}.
    QCheck combinators are available using [Spec.Gen]. *)
  type 'a gen = 'a QCheck.Gen.t

  (** ['a printer] is used to store randomly generated values *)
  type 'a printer = 'a -> string

  (** ['a spec] combines an ['a gen] and a printer *)
  type 'a spec = { gen : 'a gen; printer : 'a printer }

  (** [t] is the specification type, describing a function.
    Thus [t] declaration must end with {! (^>>) }. *)
  type ('fn, 'r) t =
    | Result : 'a printer -> ('a, 'a) t
    | Arrow : 'a spec * ('fn, 'r) t -> ('a -> 'fn, 'r) t

  (** [int] specification *)
  val int : int spec

  (** [list spec] creates a list spec for [spec] *)
  val list : 'a spec -> 'a list spec

  (** [(^>) x y] combines spec [x] and [y] to create [x -> y] *)
  val ( ^> ) : 'a spec -> ('b, 'c) t -> ('a -> 'b, 'c) t

  (** [(^>>) x res] combines a specification and printer for the result type *)
  val ( ^>> ) : 'a spec -> 'b printer -> ('a -> 'b, 'b) t
end

module Test : sig
  type t

  (** [make path spec name f] builds a test
      @param count number of application
      @param rand random state
      @param path where snapshots are stored 
      @param spec function specification
      @param name function name
      @param f function to snapshot
  *)
  val make :
    ?count:int ->
    ?rand:Random.State.t ->
    path:string ->
    spec:('a -> 'b, 'c) Spec.t ->
    name:string ->
    ('a -> 'b) ->
    t
end

(**/**)

module Snapshot : sig
  val show : ('a, 'b) Spec.t -> Memory.Snapshot.t -> string

  val make : ?rand:Random.State.t -> Test.t -> Memory.Snapshot.t
end

(**/**)

(** {2 Runner test}

    Runner has tree mode:
    
    - Interactive:

    Interactive mode provides an interactive runner, displaying differences between
    old and new snapshots. Promoting new version is proposed at every diff, exit
    when new diff is not promoted, considered as an error. 

    Example:

    {[
      let test =
        let spec = OSnap.Spec.(int ^> int ^>> int) in
        Osnap.Test.make ~count:1 ~path:".osnap/add" ~spec (+)

      (* .osnap/add:

         f 5 6 = 11; *)

      (* Then, the function under test is updated *)
      let test =
        let spec = OSnap.Spec.(int ^> int ^>> int) in
        Osnap.Test.make ~count:1 ~path:".osnap/add" ~spec (fun _ _ -> 0)

      let _ = Runner.(run_tests ~mode:Interactive [test])

      (* .osnap/add
         -| f 5 6 = 11;
         +| f 5 6 = 0

         Promote changes ? [Y\n] *)
    ]}

    - Promote:

    Promote mode provides an automatic promotion of diffs.

    Example:
    
    {[
      let test =
        let spec = OSnap.Spec.(int ^> int ^>> int) in
        Osnap.Test.make ~count:1 ~path:".osnap/add" ~spec (+)

      (* .osnap/add:

         f 5 6 = 11; *)

      (* Then, the function under test is updated *)
      let test =
        let spec = OSnap.Spec.(int ^> int ^>> int) in
        Osnap.Test.make ~count:1 ~path:".osnap/add" ~spec (fun _ _ -> 0)

      let _ = Runner.(run_tests ~mode:Promote [test])

      (* .osnap/add has been promoted
         -| f 5 6 = 11;
         +| f 5 6 = 0
      *)
    ]}

    - Error:
    
    Error mode will raise errors on diffs.
 
    {[
      let test =
        let spec = OSnap.Spec.(int ^> int ^>> int) in
        Osnap.Test.make ~count:1 ~path:".osnap/add" ~spec (+)

      (* .osnap/add:

         f 5 6 = 11; *)

      (* Then, the function under test is updated *)
      let test =
        let spec = OSnap.Spec.(int ^> int ^>> int) in
        Osnap.Test.make ~count:1 ~path:".osnap/add" ~spec (fun _ _ -> 0)

      let _ = Runner.(run_tests ~mode:Promote [test])

      (* .osnap/add has differences, exit
         -| f 5 6 = 11;
         +| f 5 6 = 0
      *)
    ]}

*)
module Runner : sig
  type mode =
    | Interactive
        (** Interactive mode, diffs will be displayed and ask for promote *)
    | Promote  (** Promote mode, will promote every diff *)
    | Error  (** Error mode, raises error on diff *)

  (**/**)

  type res =
    [ `Passed of string
    | `Promoted of string
    | `Ignored of string
    | `Error of string * string ]
    list

  val run_tests_with_res : mode -> Test.t list -> res * int

  (**/**)

  (** [run_tests tests] executes [tests], default mode is [Error]. *)
  val run_tests : ?mode:mode -> Test.t list -> int
end
