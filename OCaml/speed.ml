(* Testing for the speed of cps vs acc vs natural recursion
 * oddly enough even with flambda we get results like the follwoing
 * ┌────────────┬────────────┬──────────┬─────────────┬─────────────┬────────────┐
 * │ Name       │   Time/Run │  mWd/Run │    mjWd/Run │    Prom/Run │ Percentage │
 * ├────────────┼────────────┼──────────┼─────────────┼─────────────┼────────────┤
 * │ cps 1      │     9.75us │   9.00kw │     120.27w │     120.27w │      0.14% │
 * │ acc 1      │     6.78us │   6.01kw │      51.83w │      51.83w │      0.10% │
 * │ nat 1      │     9.85us │   3.00kw │      17.19w │      17.19w │      0.14% │
 * │ List.map 1 │     4.43us │   3.00kw │      17.21w │      17.21w │      0.06% │
 * │ cps 2      │   204.74us │  90.01kw │  11_949.92w │  11_949.92w │      2.91% │
 * │ acc 2      │   130.53us │  60.01kw │   5_315.18w │   5_315.18w │      1.85% │
 * │ nat 2      │   130.81us │  30.00kw │   1_739.32w │   1_739.32w │      1.86% │
 * │ List.map 2 │    88.34us │  44.98kw │   2_622.00w │   2_622.00w │      1.25% │
 * │ cps 3      │ 7_039.67us │ 900.01kw │ 690_965.71w │ 690_965.71w │    100.00% │
 * │ acc 3      │ 5_171.81us │ 600.00kw │ 418_948.07w │ 418_948.07w │     73.47% │
 * │ nat 3      │ 3_391.44us │ 300.00kw │ 171_337.72w │ 171_337.72w │     48.18% │
 * │ List.map 3 │ 4_776.07us │ 584.98kw │ 385_391.23w │ 385_391.23w │     67.85% │
 * └────────────┴────────────┴──────────┴─────────────┴─────────────┴────────────┘
 *)

open Core
open Core_bench

let map_cps f xs =
  let rec loop f xs k =
    match xs with
    | x :: xs -> loop f xs (fun y -> (k (f x :: y)))
    | []      -> k []
  in loop f xs ident

let map_acc f xs =
  let rec loop f xs acc =
    match xs with
    | x :: xs -> loop f xs (f x :: acc)
    | []      -> List.rev acc
  in loop f xs []


let rec map_nat f = function
  | x :: xs -> f x :: map_nat f xs
  | []      -> []


let () =
  let test1 = List.init 1000 (fun _ -> Random.int 300) in
  let test2 = List.init 10000 (fun _ -> Random.int 3000) in
  let test3 = List.init 100000 (fun _ -> Random.int 30000) in
  Command.run (Bench.make_command [
                 Bench.Test.create ~name:"cps 1"
                                   (fun () -> map_cps succ test1);
                 Bench.Test.create ~name:"acc 1"
                                   (fun () -> map_acc succ test1);
                 Bench.Test.create ~name:"nat 1"
                                   (fun () -> map_nat succ test1);
                 Bench.Test.create ~name:"List.map 1"
                                   (fun () -> List.map ~f:succ test1);
                 Bench.Test.create ~name:"cps 2"
                                   (fun () -> map_cps succ test2);
                 Bench.Test.create ~name:"acc 2"
                                   (fun () -> map_acc succ test2);
                 Bench.Test.create ~name:"nat 2"
                                   (fun () -> map_nat succ test2);
                 Bench.Test.create ~name:"List.map 2"
                                   (fun () -> List.map ~f:succ test2);
                 Bench.Test.create ~name:"cps 3"
                                   (fun () -> map_cps succ test3);
                 Bench.Test.create ~name:"acc 3"
                                   (fun () -> map_acc succ test3);
                 Bench.Test.create ~name:"nat 3"
                                   (fun () -> map_nat succ test3);
                 Bench.Test.create ~name:"List.map 3"
                                   (fun () -> List.map ~f:succ test3);
              ])
