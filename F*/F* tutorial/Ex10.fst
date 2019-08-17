module Ex10


open FStar.All
open FStar.List.Tot
open FStar.Ref

type file = string

(* Each entry is an element in our access-control list *)
type entry =
  | Readable of string
  | Writable of string
type db = list entry

(* We define two pure functions that test whether
   the suitable permission exists in some db *)
let canWrite db file =
  Some? (tryFind (function Writable x -> x = file | _ -> false) db)

let canRead db file =
  Some? (tryFind (function Readable x | Writable x -> x = file) db)

(* The acls reference stores the current access-control list, initially empty *)
val acls : ref db
let acls = ST.alloc []

(*
   Here are two stateful functions which alter the access control list.
   In reality, these functions may themselves be protected by some
   further authentication mechanism to ensure that an attacker cannot
   alter the access control list

   F* infers a fully precise predicate transformer semantics for them.
*)


// Uncomment these types and make them precise enough to pass the test
// BEGIN: Ex10aExercise
val grant : e : entry -> ST unit (requires (fun h -> True))
                                (ensures (fun h x h' -> sel h' acls == (e :: sel h acls)))
val revoke: e : entry -> ST unit (requires (fun h -> True))
                                (ensures (fun h x h' -> not (mem e (sel h' acls))))
// END: Ex10aExercise


let grant e = acls := e :: !acls

let revoke e =
  let db = filter (fun e' -> e <> e') !acls in
  acls := db

(* Next, we model two primitives that provide access to files *)

(* We use two heap predicates, which will serve as stateful pre-conditions *)
type canRead_t f h  = canRead  (sel h acls) f == true
type canWrite_t f h = canWrite (sel h acls) f == true

(* In order to call `read`, you need to prove
   the `canRead f` permission exists in the input heap *)
assume val read :  file:string -> ST string
                                     (requires (canRead_t file))
                                     (ensures (fun h s h' -> modifies_none h h'))

(* Likewise for `delete` *)
assume val delete : file:string -> ST unit
                                     (requires (canWrite_t file))
                                     (ensures (fun h s h' -> modifies_none h h'))

(* Then, we have a top-level API, which provides protected
   access to a file by first checking the access control list.

   If the check fails, it raises a fatal exception using `failwith`.
   As such, it is defined to have effect `All`, which combines
   both state and exceptions.

   Regardless, the specification proves that `safe_Delete`
   does not change the heap.
 *)
val safe_delete: file -> All unit
                (requires (fun h -> True))
                (ensures (fun h x h' -> modifies_none h h'))


let safe_delete file =
  if canWrite !acls file
  then delete file
  else failwith "unwritable"

(* Finally, we have a top-level client program *)
val test_acls: file -> ML unit
let test_acls f =
  grant (Readable f);     (* ok *)
  let _ = read f in       (* ok --- it's in the acl *)
  //delete f;               (* not ok --- we're only allowed to read it *)
  safe_delete f;          (* ok, but fails dynamically *)
  revoke (Readable f);
  //let _ = read f in       (* not ok any more *)
  ()



noeq type point =
  | Point : x : ref int → y : ref int{addr_of y <>  addr_of x} → point

val new_point: x:int -> y:int -> ST point
  (requires (fun h -> True))
  (ensures (fun h0 p h1 ->
                modifies Set.empty h0 h1
                /\ fresh (Point?.x p) h0 h1
                /\ fresh (Point?.y p) h0 h1
                /\ Heap.sel h1 (Point?.x p) = x
                /\ Heap.sel h1 (Point?.y p) = y))
let new_point x y =
  let x = ST.alloc x in
  let y = ST.alloc y in
  Point x y

let shift_x p = Point?.x p := !(Point?.x p) + 1


val shift_x_p1: p1:point
           -> p2:point{   addr_of (Point?.x p2) <> addr_of (Point?.x p1)
                       /\ addr_of (Point?.y p2) <> addr_of (Point?.x p1) }
           -> ST unit
    (requires (fun h -> Heap.contains h (Point?.x p2)
                    /\  Heap.contains h (Point?.y p2)))
    (ensures (fun h0 _ h1 -> modifies (only (Point?.x p1)) h0 h1))

let shift_x_p1 p1 p2 =
    let p2_0 = !(Point?.x p2), !(Point?.y p2)  in //p2 is initially p2_0
    shift_x p1;
    let p2_1 = !(Point?.x p2), !(Point?.y p2) in
    admit(); //fix the precondition and remove the admit
    assert (p2_0 = p2_1)                         //p2 is unchanged
