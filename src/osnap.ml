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
    rand : Random.State.t;
  }

  type t = Test : ('a, 'b, 'c) cell -> t

  let make ?(count = 10) ?(rand = Random.State.make_self_init ()) ?path ~spec
      ~name f =
    let path = Option.value ~default:(Common.opt_path name) path in
    Test { path; spec; f; count; name; rand }
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

  type encoding = Snapshot.mode = Marshal | Data_encoding

  let mode_from_string s =
    let s = String.lowercase_ascii s in
    match s with
    | "interactive" -> Interactive
    | "promote" -> Promote
    | "error" | _ -> Error

  let encoding_from_string s =
    let s = String.lowercase_ascii s in
    match s with "data_encoding" -> Data_encoding | "marshal" | _ -> Marshal

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
    | `Error (_, msg) ->
        Format.fprintf fmt "@.--- %a %s@.@.%s@.@." pp_failure color sep msg
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
      "Do you want to promote this new snapshot? [Y\\n]"

  let rec take_input fmt () =
    let () = input_msg fmt () in
    match read_line () with
    | "Y" | "" -> true
    | "n" -> false
    | _ -> take_input fmt ()

  let interactive ~path ~diff ~name fmt encoding spec (snapshot, snapshot_str) =
    match diff with
    | Diff.Same -> `Passed name
    | _ ->
        let () = Format.pp_print_string fmt snapshot_str in
        if take_input fmt () then
          let () = Snapshot.encode ~spec ~mode:encoding ~path snapshot in
          `Promoted name
        else `Ignored name

  let error ~path ~diff ~name =
    match diff with
    | Diff.Same -> `Passed name
    | Diff.(New _) ->
        let msg =
          Printf.sprintf
            "Error: no previous snapshot at %s"
            (Common.full_path path)
        in
        `Error (name, msg)
    | Diff.Diff diff -> `Error (name, diff)

  let promote ~path ~diff ~name encoding spec snapshot =
    match diff with
    | Diff.Same -> `Passed name
    | Diff.(New _) | Diff.Diff _ ->
        let () = Snapshot.encode ~spec ~mode:encoding ~path snapshot in
        `Promoted name

  let map o f = Option.map f o

  let ( >>| ) = map

  type ('fn, 'r) intermediate_snapshot = {
    snap : ('fn, 'r) Snapshot.t;
    text : string;
    path : string;
  }

  let prev spec mode path : ('a, 'b) intermediate_snapshot option =
    Snapshot.decode_opt ~spec ~mode ~path () >>| fun snap ->
    Snapshot.to_string spec snap |> fun text ->
    path ^ ".prev" |> fun path -> { snap; text; path }

  let next ~f ~rand ~name ~spec ~count ~path prev =
    let snap =
      match prev with
      | Some { snap; _ } -> Snapshot.create_from_snapshot snap f
      | None -> Snapshot.create ~rand ~name ~spec ~f count
    in
    let text = Snapshot.to_string spec snap in
    let path = path ^ ".next" in
    { snap; text; path }

  let diff path prev next =
    let diff_path = path ^ ".diff" in
    ( Diff.diff
        ~path:diff_path
        ~prev:(prev >>| fun prev -> prev.path)
        ~next:next.path,
      diff_path )

  let prepare_run prev next =
    Option.iter (fun prev -> Common.write prev.path prev.text) prev ;
    Common.write next.path next.text

  let clean_run prev next diff_path =
    let clean fd = if Sys.file_exists fd then Sys.remove fd in
    Option.iter (fun prev -> clean prev.path) prev ;
    clean next.path ;
    clean diff_path

  let run encoding mode fmt test =
    let Test.(Test { spec; path; name; rand; count; f; _ }) = test in

    let prev = prev spec encoding path in
    let next = next ~f ~rand ~name ~spec ~count ~path prev in
    let () = prepare_run prev next in

    let (diff, diff_path) = diff path prev next in

    let res =
      match mode with
      | Error -> error ~path ~diff ~name
      | Promote -> promote ~path ~diff ~name encoding spec next.snap
      | Interactive ->
          interactive ~path ~diff ~name fmt encoding spec (next.snap, next.text)
    in

    let () = clean_run prev next diff_path in
    res

  let run_tests_with_res encoding mode out tests : res * int =
    let res = List.map (run encoding mode out) tests in
    let status =
      if List.exists (function `Error _ -> true | _ -> false) res then 1
      else 0
    in
    (res, status)

  let run_tests ?(encoding = Marshal) ?(mode = Error)
      ?(out = Format.std_formatter) ?(color = true) tests =
    let (res, status) = run_tests_with_res encoding mode out tests in
    let () = pp_res out ~color res in
    status

  module Cli = struct
    type args = { mode : mode; color : bool; encoding : encoding }

    let parse argv =
      let mode = ref "" in
      let color = ref false in
      let encoding = ref "" in

      let options =
        Arg.align
          [
            ("--mode", Arg.Set_string mode, " set runner mode");
            ("-m", Arg.Set_string mode, " set runner mode");
            ("--color", Arg.Set color, " set color output");
            ("-c", Arg.Set color, " set color output");
            ("-encoding", Arg.Set_string encoding, " set encoding mode");
            ("-e", Arg.Set_string encoding, " set encoding mode");
          ]
      in
      let () = Arg.parse_argv argv options (fun _ -> ()) "run osnap suite" in
      let mode = mode_from_string !mode in
      let encoding = encoding_from_string !encoding in
      { mode; color = !color; encoding }
  end

  let run_tests_main ?(argv = Sys.argv) tests =
    try
      let cli_args = Cli.parse argv in

      exit (run_tests ~mode:cli_args.mode ~color:cli_args.color tests)
    with
    | Arg.Bad msg ->
        print_endline msg ;
        exit 1
    | Arg.Help msg ->
        print_endline msg ;
        exit 0
end
