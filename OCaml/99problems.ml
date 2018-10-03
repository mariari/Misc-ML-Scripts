open Core
open Core_bench
(* problem 32 *)
let rec gcd a b = if b = 0 then a else gcd b (a mod b)

(* problem 33 *)
let coprime a b = gcd a b = 1

(* problem 34 *)
let phi_old n =
  List.range 0 n
  |> List.map ~f:(coprime n)
  |> List.count ~f:ident

(* problem 36 - use CPS later, and send in a prime list later *)
let factors n =
  let rec aux x n =
    if n = 1 then
      []
    else
      if n mod x = 0 then
        match aux x (n / x) with
        | (x', num) :: xs when x' = x -> (x, succ num) :: xs
        | xs                          -> (x, 1)        :: xs
      else
        aux (succ x) n
  in aux 2 n


let phi n =
  factors n
  |> List.fold_right ~f:(fun (x, num) acc -> Int.(acc * x - 1 * pow x (num - 1)))
                     ~init: 1

(* let () =
 *   Command.run (Bench.make_command [
 *                  Bench.Test.create ~name:"phi-fast"
 *                    (fun () -> phi 9999999);
 *                  Bench.Test.create ~name:"phi-old"
 *                    (fun () -> phi_old 9999999)
 *               ]) *)
