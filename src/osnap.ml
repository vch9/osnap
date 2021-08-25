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

module Spec = Spec

module Test = struct
  type ('a, 'b, 'c) cell = {
    path : string;
    name : string;
    spec : ('a -> 'b, 'c) Spec.t;
    f : 'a -> 'b;
    count : int;
    rand : Random.State.t option;
  }

  type t = Test : ('a, 'b, 'c) cell -> t

  let make ?(count = 10) ?rand ~path ~spec ~name f =
    Test { path; spec; f; count; name; rand }
end

module Snapshot = struct
  open Test
  module M = Memory

  let rec decode_applications :
      type a b.
      (a, b) Spec.t -> (a, b) Interpreter.args * string -> string * string =
   fun spec (args, res) ->
    let open Interpreter in
    let open Spec in
    match (args, spec) with
    | (Cons (x, xs), Arrow ({ printer; _ }, ys)) ->
        let s = (default_printer printer) x in
        let (x, y) = decode_applications ys (xs, res) in
        ("  " ^ s ^ x, y)
    | (Nil, Result printer) ->
        let x = M.Encode.from_string res in
        ("", printer x)
    | _ -> assert false

  let show spec snapshot =
    let name = M.Snapshot.name snapshot in
    let applications =
      M.Snapshot.applications snapshot
      |> List.map (fun (args, res) ->
             decode_applications spec (M.Encode.from_string args, res))
    in
    List.fold_left
      (fun acc (args, res) ->
        Printf.sprintf "%s%s  =>  %s\n%s" name args res acc)
      ""
      applications

  let make ?rand (Test { spec; f; count; name; rand = rand'; _ }) =
    let rand = match (rand, rand') with (None, x) -> x | (x, _) -> x in
    let spec_to_args =
      Option.fold
        ~none:Interpreter.spec_to_args
        ~some:(fun rand -> Interpreter.Internal_for_tests.spec_to_args rand)
        (* For testing only *)
        rand
    in
    let applications =
      List.init count (fun _ ->
          let args = spec_to_args spec in
          let res = Interpreter.(args_to_expr (Fun f) args |> interpret) in
          (M.Encode.to_string args [], M.Encode.to_string res []))
    in
    M.Snapshot.build name applications

  let next test snapshot =
    let Test.(Test { f; name; _ }) = test in
    match snapshot with
    | None -> make test
    | Some prev ->
        let decoded_applications =
          M.Snapshot.applications prev
          |> List.map (fun (x, y) ->
                 (M.Encode.from_string x, M.Encode.from_string y))
        in
        let new_applications =
          List.map
            (fun (args, _) ->
              let res = Interpreter.(args_to_expr (Fun f) args |> interpret) in
              (M.Encode.to_string args [], M.Encode.to_string res []))
            decoded_applications
        in
        M.Snapshot.build name new_applications
end

module Color = struct
  type color = [ `Red | `Green ]

  let int_of_color = function `Red -> 1 | `Green -> 2

  let pp_str ?(bold = false) fmt (color : color) s =
    let open Format in
    let n = int_of_color color in
    let () =
      if bold then fprintf fmt "\x1b[3%d;1m" n else fprintf fmt "\x1b[3%dm" n
    in
    let () = pp_print_string fmt s in
    fprintf fmt "\x1b[0m"
end

module Runner = struct
  type mode = Interactive | Promote | Error

  type res =
    [ `Passed of string
    | `Promoted of string
    | `Ignored of string
    | `Error of string * string ]
    list

  let get_passed xs = List.filter (function `Passed _ -> true | _ -> false) xs

  let get_promoted xs =
    List.filter (function `Promoted _ -> true | _ -> false) xs

  let get_ignored xs =
    List.filter (function `Ignored _ -> true | _ -> false) xs

  let get_errors xs = List.filter (function `Error _ -> true | _ -> false) xs

  let sep = String.make 68 '-'

  let pp_failure fmt color =
    let s = "failure" in
    if color then Color.(pp_str ~bold:true fmt `Red s)
    else Format.pp_print_string fmt s

  let pp_success fmt color =
    let s = "success" in
    if color then Color.(pp_str fmt `Green "success")
    else Format.pp_print_string fmt s

  let pp_error fmt ~color = function
    | `Error (_, x) ->
        Format.fprintf fmt "@.--- %a %s@.@.%s@.@." pp_failure color sep x
    | _ -> ()

  let pp_recap fmt ~color passed promoted ignored errors =
    let open Format in
    let n =
      List.(length passed + length promoted + length ignored + length errors)
    in
    let strs =
      [
        (let n = List.length passed in
         if n > 0 then Option.some @@ Printf.sprintf "%d passed" n else None);
        (let n = List.length errors in
         if n > 0 then Option.some @@ Printf.sprintf "%d error(s)" n else None);
        (let n = List.length promoted in
         if n > 0 then Option.some @@ Printf.sprintf "%d promoted" n else None);
        (let n = List.length ignored in
         if n > 0 then Option.some @@ Printf.sprintf "%d ignored" n else None);
      ]
      |> List.filter_map (fun x -> x)
    in

    let pp_aux fmt strs =
      pp_print_list
        ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
        pp_print_string
        fmt
        strs
    in

    let pp_res fmt errors =
      if List.length errors > 0 then pp_failure fmt color
      else pp_success fmt color
    in

    fprintf
      fmt
      "-----------%s@.%a: ran %d test%s (%a)@."
      sep
      pp_res
      errors
      n
      (if n = 1 then "" else "s")
      pp_aux
      strs

  let pp_res fmt ~color xs =
    let passed = get_passed xs in
    let promoted = get_promoted xs in
    let ignored = get_ignored xs in
    let errors = get_errors xs in

    let () = match errors with x :: _ -> pp_error fmt ~color x | _ -> () in

    pp_recap fmt ~color passed promoted ignored errors

  let input_msg fmt () =
    let open Format in
    fprintf
      fmt
      "@.@.%a@."
      pp_print_string
      "Do you want to promote these diff? [Y\\n]"

  let rec take_input fmt () =
    let () = input_msg fmt () in
    match read_line () with
    | "Y" | "" -> true
    | "n" -> false
    | _ -> take_input fmt ()

  let interactive fmt diff name path snapshot =
    match diff with
    | Diff.Same -> `Passed name
    | _ ->
        let msg =
          match diff with
          | Diff.(New s) -> s
          | Diff.(Diff s) -> s
          | _ -> assert false
        in
        let () = Format.pp_print_string fmt msg in
        if take_input fmt () then
          let () = Memory.Snapshot.write path snapshot in
          `Promoted name
        else `Ignored name

  let error diff name =
    match diff with
    | Diff.Same -> `Passed name
    | Diff.(New s) ->
        let msg = Printf.sprintf "Error: no previous snapshot, new:\n%s" s in
        `Error (name, msg)
    | Diff.(Diff s) -> `Error (name, s)

  let promote diff name path snapshot =
    match diff with
    | Diff.Same -> `Passed name
    | Diff.(New _) | Diff.(Diff _) ->
        let () = Memory.Snapshot.write path snapshot in
        `Promoted name

  let run mode fmt test =
    let Test.(Test { spec; path; name; _ }) = test in
    let prev = Memory.Snapshot.read path in
    let prev_str =
      Option.fold
        ~none:None
        ~some:(fun x -> Option.some @@ Snapshot.show spec x)
        prev
    in

    let next = Snapshot.next test prev in
    let next_str = Snapshot.show spec next in

    let diff = Diff.diff prev_str next_str in
    match mode with
    | Error -> error diff name
    | Promote -> promote diff name path next
    | Interactive -> interactive fmt diff name path next

  let run_tests_with_res mode out tests : res * int =
    let res = List.map (run mode out) tests in
    let status =
      if List.exists (function `Error _ -> true | _ -> false) res then 1
      else 0
    in
    (res, status)

  let run_tests ?(mode = Error) ?(out = Format.std_formatter) ?(color = true)
      tests =
    let (res, status) = run_tests_with_res mode out tests in
    let () = pp_res out ~color res in
    status
end
