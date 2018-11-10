open Core
open Core_bench

(* converts a list, xs, to a hashtbl, In reality this is a bag *)
let to_hash xs =
  let hash = Hashtbl.Poly.create ~growth_allowed:false ~size:(List.length xs) () in
  let f x = function
    | Some x -> succ x
    | None   -> 1
  in
  List.iter xs ~f:(fun x -> Hashtbl.update hash x (f x));
  hash

(* fast way of doing list difference, O(n + m) *)
let rec diff xs ys =
  let hash = to_hash ys in
  let rem_or_del x =
    Hashtbl.change hash x ~f:(function
        | None    -> None
        | Some 1  -> None
        | Some x  -> Some (pred x) )
  in
  List.filter xs ~f:(fun x ->
      if Hashtbl.mem hash x then begin
        rem_or_del x;
        false
      end else
        true)

(* Removes a specific element from a list *)
let remove xs ~ele =
  let rec loop acc = function
    | []                   -> List.rev acc
    | x :: xs when x = ele -> List.append (List.rev acc) xs
    | x :: xs              -> loop (x :: acc) xs
  in
  loop [] xs

(* Slow way to do list difference, should be faster for lists below length 50, O(n²) *)
let rec diff' xs = function
  | []    -> xs
  | y::ys -> diff' (remove ~ele:y xs) ys


(* let () =
 *   let test1 = List.init 1000 (fun _ -> Random.int 300) in
 *   let test2 = List.init 1000 (fun _ -> Random.int 300) in
 *   Command.run (Bench.make_command [
 *                  Bench.Test.create ~name:"diff O(n²)"
 *                                    (fun () -> diff' test1 test2);
 *                  Bench.Test.create ~name:"diff O(n + m)"
 *                                    (fun () -> diff test1 test2);
 *               ]) *)
