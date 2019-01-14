open Core
open Core_bench

(* Could save hash table space, by calling List.dedup_and_sort
 * however, this would move the to_hash function to O(n × log n)
 *)
(** converts a list, xs, to a hashtbl, In reality this is a bag
 *  also takes a module with a hash function and a competitor
 *  this runs in O(n) time, however the hash_table will take up O(n)
 *  sort_space_by_dedup, dedups the list, before creating the size of the
 *  hash_table/bag, saving hashtable space, at the cost of O(n log n)
 *)
let to_hash
      (type a)
      (module Key_module : Hashtbl_intf.Key with type t = a)
      ?(save_space_by_dedup = false)
      (xs : a list) =
  let size =
    if save_space_by_dedup then
      xs
      |> List.dedup_and_sort ~compare:Key_module.compare
      |> List.length
    else (List.length xs)
  in
  let hash = Hashtbl.create (module Key_module) ~growth_allowed:false ~size in
  let f    = Option.value_map ~f:succ ~default:1 in
  List.iter xs ~f:(Hashtbl.update hash ~f);
  hash

(** fast way of doing list difference, O(n + m)
 *  also takes a module with a hash function and a competitor
 *)
let rec diff val_module xs ys =
  let hash = to_hash val_module ys in
  List.filter xs ~f:(fun x ->
      if Hashtbl.mem hash x then begin
        Hashtbl.decr ~remove_if_zero:true hash x;
        false
      end else
        true)

(* Removes a specific element from a list, uses a polymorphic competitor *)
let remove xs ~ele =
  let rec loop acc = function
    | []                   -> List.rev acc
    | x :: xs when x = ele -> List.rev_append acc xs
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
