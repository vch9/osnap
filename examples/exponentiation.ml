(* let rec exponentiation x n =
 *   if n = 0 then 1 else if n = 1 then x else exponentiation x (n - 1) * x *)

let rec binary_expo x n =
  if n = 0 then 1
  else if n mod 2 = 0 then
    let tmp = binary_expo x (n / 2) in
    tmp * tmp
  else x * binary_expo x (n - 1)

let test =
  let open Osnap in
  let small_int =
    Spec.
      {
        gen = QCheck.Gen.small_int;
        printer = Some string_of_int;
        encoding = None;
      }
  in
  let spec = Spec.(small_int ^> small_int ^>> Result.int) in
  let path = ".osnap/exponentiation" in

  Test.make ~spec ~path ~count:5 ~name:"exponentiation" binary_expo

let _ = Osnap.Runner.(run_tests ~mode:Interactive [ test ])
