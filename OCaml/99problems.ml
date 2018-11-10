open Core
open Core_bench
(* problem 32 *)
let rec gcd a b = if b = 0 then a else gcd b (a mod b)

(* problem 33 *)
let coprime a b = gcd a b = 1

(* problem 34 *)
let phi_old n =
  Sequence.range 0 n
  |> Sequence.map ~f:(coprime n)
  |> Sequence.count ~f:ident

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

(* book solution... for testing speed *)
let phi_site n =
  let rec count_coprime acc d =
    if d < n then count_coprime (if coprime n d then acc + 1 else acc) (d + 1)
    else acc
  in
  if n = 1 then 1 else count_coprime 0 1


let phi n =
  factors n
  |> List.fold_left ~f:(fun acc (x, num) -> Int.(acc * (x - 1) * pow x (num - 1)))
                    ~init: 1

(* not surpassing that phi_site is 3x faster and has the lowest alloc rate *)
(* since converting phi_old to Sequence, it's only slightly slower *)
(* let () =
 *   Command.run (Bench.make_command [
 *                  Bench.Test.create ~name:"phi-fast"
 *                                    (fun () -> phi 99999);
 *                  Bench.Test.create ~name:"phi-old"
 *                                    (fun () -> phi_old 99999);
 *                  Bench.Test.create ~name:"phi-site"
 *                                    (fun () -> phi_site 99999)
 *               ]) *)
