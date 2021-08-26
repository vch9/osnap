let rec exponentiation x n =
  if n = 0 then 1 else if n = 1 then x else exponentiation x (n - 1) * x

let test =
  let open Osnap in
  let small_int = Spec.{ gen = Gen.small_int; printer = Some string_of_int } in
  let spec = Spec.(small_int ^> small_int ^>> string_of_int) in
  let path = ".osnap/exponentiation" in

  Test.make ~spec ~path ~count:5 ~name:"exponentiation" exponentiation

let _ = Osnap.Runner.(run_tests ~mode:Interactive [ test ])
