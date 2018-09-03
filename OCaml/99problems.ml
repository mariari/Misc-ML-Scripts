open Core

(* problem 32 *)
let rec gcd a b = if b = 0 then a else gcd b (a mod b)

(* problem 33 *)
let coprime a b = gcd a b = 1

(* for some reason core ident signal *)
(* Error: Reference to undefined global `Core' *)
let ident x = x

(* problem 34 *)
let phi n =
  List.range 0 n
  |> List.map ~f:(coprime n)
  |> List.count ~f:ident
