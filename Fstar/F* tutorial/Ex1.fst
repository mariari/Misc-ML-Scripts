module Ex1

open FStar.Exn
open FStar.All
open FStar.Ref

type filename = string

(** [canWrite] is a function specifying whether a file [f] can be written *)
let canWrite (f:filename) =
  match f with
  | "demo/tempfile" -> true
  | _               -> false


(** [canRead] is also a function ... *)
let canRead (f:filename) =
  canWrite f               (* writeable files are *)
   || f="demo/README"       (* and so is demo/README *)


val read : f:filename{canRead f} -> ML string
let read f =
  FStar.IO.print_string ("Dummy read of file " ^ f ^ "\n");
  f

val write : f:filename{canWrite f} -> string -> ML unit
let write f s = FStar.IO.print_string ("Dummy write of string " ^ s ^ " to file " ^ f ^ "\n")


let passwd : filename = "demo/password"
let readme : filename = "demo/README"
let tmp    : filename = "demo/tempfile"

val staticChecking : unit -> ML unit
let staticChecking () =
  let v1 = read tmp in
  let v2 = read readme in
  // let v3 = read passwd in // invalid read, fails type-checking
  write tmp "hello!"
  // ; write passwd "junk" // invalid write , fails type-checking

exception InvalidRead

val checkedRead : filename -> ML string
let checkedRead f =
  if canRead f then read f else raise InvalidRead

exception InvalidWrite

val checkedWrite : filename -> string -> ML unit

let checkedWrite f s = if canWrite f
                       then write f s
                       else raise InvalidWrite

let dynamicChecking () =
  let v1 = checkedRead tmp in
  let v2 = checkedRead readme in
  let v3 = checkedRead passwd in (* this raises exception *)
  checkedWrite tmp "hello!";
  checkedWrite passwd "junk" (* this raises exception *)

val is_positive : int -> Tot bool
let is_positive i = i > 0

val max : int -> int -> Tot int
let max i1 i2 = if i1 > i2 then i1 else i2

let _ = assert (forall x y. max x y >= x
                    && max x y >= y
                    && (max x y = x || max x y = y))

val new_point: x:int -> ST (ref int)
  (requires (fun h -> True))
  (ensures (fun h0 p h1 ->
                 fresh p h0 h1))
let new_point (x : int) =
  let x = ST.alloc x in
  x


//val new_counter: int -> ST (unit -> St int)
val new_counter : c : ref int
                -> unit
                -> ST int
  (requires (fun h -> Heap.contains h c))
  (ensures (fun h0 x h1 -> modifies (only c) h0 h1
                          /\ equal_dom h0 h1))
let new_counter c =
  //let c = ST.alloc c in
 fun () -> c := !c + 1; !c


// Trials in dealing with the double encoding bug
val test'' : x : int
    -> STATE (_ : unit -> STATE int
                        (fun p h0 ->
                          (forall (a : ref int) (h1:  heap).
                            contains h0 a
                          /\ modifies (only a) h0 h1
                          /\ equal_dom h0 h1 ==>
                          (forall (return_val : int).
                            return_val == 1 + sel h0 a ==>
                            return_val == sel h1 a ==>  p return_val h1))))
        (fun p h0 ->
            (forall (a: ref int) (h1:  heap).
                fresh a h0 h1 /\ modifies Set.empty h0 h1 /\ sel h1 a == x
                ==>
                (forall (return_val : (_: unit -> STATE int (fun p h0 ->
                          (forall (_ : unit).
                            contains h0 a
                          /\ modifies (only a) h0 h1
                          /\ equal_dom h0 h1 ==>
                          (forall (return_val : int).
                            return_val == 1 + sel h0 a ==>
                            return_val == sel h1 a ==>  p return_val h1))))).
//                      return_val == (fun () -> a := !a + 1; !a) ==>
                      p return_val h1)))


val test''' : x : int
    -> STATE (_ : unit -> ST int (requires (fun _ -> True))
                        (ensures (fun h0 x h1 ->
                          (forall (a : ref int).
                            contains h0 a
                          /\ modifies (only a) h0 h1
                          /\ equal_dom h0 h1
                          ==> x == sel h1 a /\ x == (sel h0 a + 1)))))
        (fun p h0 ->
            (forall (a: ref int) (h1:  heap).
                fresh a h0 h1 /\ modifies Set.empty h0 h1 /\ sel h1 a == x
                ==>
                (forall (return_val : (_ : unit ->  ST int (requires (fun _ -> True))
                                                   (ensures (fun h0 x h1 ->
                                                     (forall (a : ref int).
                                                     contains h0 a
                                                   /\ modifies (only a) h0 h1
                                                   /\ equal_dom h0 h1
                                                   ==> x == sel h1 a /\ x == (sel h0 a + 1)))))) .
                      return_val == (fun () -> a := !a + 1; !a) ==>
                      p return_val h1)))

// with ML
val new_counter' : int -> ML (unit -> ML int)
let new_counter' x =
 let y = ST.alloc x in
 fun () ->
   y := !y + 1;
   !y

// with ST
type fn = unit -> St int

val bar: int -> St fn
let bar x =
  let r = alloc x in
  ((fun () -> r := !r + 1; !r) <: fn)


val factorial : nat -> Tot (y:int{y >= 1})
let rec factorial n =
  if n = 0
  then 1
  else n `op_Multiply` (factorial (n - 1))


val factorial_dv : int -> Dv int
let rec factorial_dv n =
  let open FStar.Mul in
  if n = 0
  then 1
  else n * (factorial_dv (n - 1))

//val fibonacci : int → y:int{y >= 1}
val fibonacci : x:int -> nat
let rec fibonacci n =
  if n <= 1 then 1 else fibonacci (n - 1) + fibonacci (n - 2)


// replace later with tactics?
val factorial_is_positive: x:nat -> GTot (u:unit{factorial x > 0})
let rec factorial_is_positive x =
  match x with
  | 0 -> ()
  | _ -> factorial_is_positive (x - 1)

val factorial_is_greater_than_arg3: x:nat{x > 2} -> Lemma (factorial x > x)

val factorial_is_greater_than_arg4: x:nat -> Lemma (requires (x > 2))
                                                (ensures (factorial x > x))

let rec factorial_is_greater_than_arg4 x =
  match x with
  | 3 -> ()
  | _ -> factorial_is_greater_than_arg4 (x - 1)

val fibonacci_greater_than_arg : n:nat{n >= 2} -> Lemma (fibonacci n >= n)
let rec fibonacci_greater_than_arg x =
  match x with
  | 2 → ()
  | _ → fibonacci_greater_than_arg (x - 1)


val append : l1 : list 'a
           → l2 : list 'a
           → l  : list 'a{List.Tot.length l = List.Tot.length l1 + List.Tot.length l2}
let rec append l1 l2 = match l1 with
  | []      → l2
  | hd :: tl → hd :: append tl l2


val append_weak : list 'a -> list 'a -> Tot (list 'a)
let rec append_weak l1 l2 = match l1 with
  | []      → l2
  | hd :: tl → hd :: append_weak tl l2

val append_lengths : l1 : list 'a
                   → l2 : list 'a
                   → Lemma (List.Tot.length (append_weak l1 l2) == (List.Tot.length l1 + List.Tot.length l2))

let rec append_lengths l1 l2 = match l1 with
  | []      → ()
  | hd :: tl → append_lengths tl l2


val append_expanded : #a:Type -> list a -> list a -> Tot (list a)
let rec append_expanded #a l1 l2 = match l1 with
  | [] -> l2
  | hd :: tl -> hd :: append #a tl l2


val mem : #a:eqtype -> a -> list a -> Tot bool
let rec mem #a x xs =
  match xs with
  | [] -> false
  | hd :: tl -> hd = x || mem x tl

val append_mem: #t:eqtype
	      -> l1:list t
              -> l2:list t
              -> Lemma (requires True)
                       (ensures (forall a. mem a (append l1 l2) = (mem a l1 || mem a l2)))
                       [SMTPat (append l1 l2)]
let rec append_mem #t l1 l2 = match l1 with
  | [] -> ()
  | hd::tl -> append_mem tl l2


val reverse : list 'a -> Tot (list 'a)
let rec reverse = function
  | [] -> []
  | hd :: tl -> append (reverse tl) [hd]



let snoc l h = append l [h]

val snoc_cons: l:list 'a -> h:'a -> Lemma (reverse (snoc l h) == h :: reverse l)
let rec snoc_cons l h = match l with
  | [] -> ()
  | hd :: tl -> snoc_cons tl h

val rev_involutive: l:list 'a -> Lemma (reverse (reverse l) == l)
let rec rev_involutive l = match l with
  | [] -> ()
  | hd :: tl ->
    // (1) [reverse (reverse tl) == tl]
    rev_involutive tl;
    // (2) [reverse (append (reverse tl) [hd]) == h :: reverse (reverse tl)]
    snoc_cons (reverse tl) hd
    // These two facts are enough for Z3 to prove the lemma:
    //   reverse (reverse (hd :: tl))
    //   =def= reverse (append (reverse tl) [hd])
    //   =(2)= hd :: reverse (reverse tl)
    //   =(1)= hd :: tl
    //   =def= l

val rev_injective : l1:list 'a -> l2:list 'a
                -> Lemma (requires (reverse l1 == reverse l2))
                         (ensures  (l1 == l2))

let rec rev_injective l1 l2 =
  rev_involutive l1;
  rev_involutive l2

val snoc_inject : l1 : list 'a
                → h1 : 'a
                → l2 : list 'a
                → h2 : 'a
                → Lemma (requires (snoc l1 h1 == snoc l2 h2))
                        (ensures (l1 == l2 /\ h1 == h2))
let rec snoc_inject l1 h1 l2 h2 =
  match (l1, l2) with
  | [], []             -> ()
  | (_ :: tl1, _ :: tl2) → snoc_inject tl1 h1 tl2 h2


val rev_inject : l1:list 'a → l2:list 'a
               → Lemma (requires (reverse l1 == reverse l2))
                       (ensures  (l1 == l2))
let rec rev_inject l1 l2 =
  match l1, l2 with
  | [], [] -> ()
  | h1 :: t1, h2 :: t2 ->
    // reverse l1 = append (reverse t1) [hd1] = snoc (reverse t1) h1
    // reverse l2 = append (reverse t2) [hd2] = snoc (reverse t2) h2
    // (1) snoc_inject gives us
    // snoc (reverse t1) h1 == snoc (reverse t2) h2
    // ===> reverse t1 == reverse t2 /\ h1 == h2
    snoc_inject (reverse t1) h1 (reverse t2) h2;
    // (2) by induction hypothesis
    // reverse t1 == reverse t2 ===> t1 == t2
    rev_inject t1 t2
    // reverse (h1 :: t1), reverse (h2 :: t2)
    // =def= snoc (reverse t1) h1, snoc (reverse t2) h2
    // (1) reverse t1 == reverse t2 /\ h1 == h2
    // (2) t1 == t2
    // ∴ h1 :: t1 == h2 :: t2

val map : ('a -> 'b) -> list 'a -> list 'b
let rec map f l = match l with
  | [] -> []
  | hd::tl -> f hd :: map f tl

//let test l = map (fun x -> FStar.IO.print_any x) l


type option 'a =
   | None : option 'a
   | Some : v:'a -> option 'a


val find : f : ('a -> bool)
         → list 'a
         → x : (option (y : 'a{f y}))
let rec find f l = match l with
  | [] -> None
  | hd :: tl -> if f hd then Some hd else find f tl


val find_prop : f : ('a -> bool)
              → l : list 'a
              -> Lemma (match find f l with
                      | None   → true
                      | Some v → f v)
let rec find_prop f = function
  | [] → ()
  | x :: xs → find_prop f xs
