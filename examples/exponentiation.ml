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
        gen = QCheck.Gen.(0 -- 5);
        printer = Some string_of_int;
        encoding = Some Data_encoding.int31;
      }
  in
  let spec = Spec.(small_int ^> small_int ^>> Result.int) in

  Test.make ~spec ~count:5 ~name:"exponentiation" binary_expo

let _ = Osnap.Runner.(run_tests ~encoding:Data_encoding ~mode:Error [ test ])
