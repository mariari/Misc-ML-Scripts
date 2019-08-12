module Ex5

open Ex1
open Ex4

// val ackermann_swap: n:nat -> m:nat -> Tot nat (decreases %[m;n])
val ackermann_swap: n:nat -> m:nat -> Tot nat (decreases (LexCons m (LexCons n LexTop)))
let rec ackermann_swap n m =
   if m = 0 then n + 1
   else if n = 0 then ackermann_swap 1 (m - 1)
   else ackermann_swap (ackermann_swap (n - 1) m)
                       (m - 1)


val rev : l1:list 'a -> l2:list 'a -> Tot (list 'a) (decreases l2)
let rec rev l1 l2 =
  match l2 with
  | []     -> l1
  | hd :: tl -> rev (hd :: l1) tl

// This is needed, as for the same reason and foldl, I need to reason over l2
// only having [] will not let me do this!
val rec_is_ok_helper : l1 : list 'a
                     → l2 : list 'a
                     → Lemma (ensures (rev l1 l2 == fold_left consFlip l1 l2)) (decreases l2)

let rec rec_is_ok_helper l1 l2 =
    match l2 with
    | []      → ()
    | x :: xs -> rec_is_ok_helper (x :: l1) xs

val append_empty_is_id : l1 : list 'a → Lemma (ensures (append l1 [] == l1))
let rec append_empty_is_id = function
  | []     -> ()
  | _ :: xs -> append_empty_is_id xs

val rev_is_reverse_app : l   : list 'a
                       → acc : list 'a
                       → Lemma (rev acc l == append (reverse l) acc)
let rev_is_reverse_app l acc =
  fold_left_cons_is_reverse l acc;
  rec_is_ok_helper acc l


val rev_is_ok : l:list 'a -> Lemma (rev [] l == reverse l)
let rev_is_ok l =
  rev_is_reverse_app l [];
  append_empty_is_id (reverse l)


val fib : nat -> nat -> n:nat -> Tot nat (decreases n)
let rec fib a b n =
  match n with
  | 0 -> a
  | _ -> fib b (a+b) (n-1)

val fib_is_ok_helper : n : nat
                     → k : nat
                     → Lemma (fib (fibonacci k) (fibonacci (k + 1)) n == fibonacci (k + n))
let rec fib_is_ok_helper n k =
  if n <= 0 then () else fib_is_ok_helper (n - 1) (k + 1)

val fib_is_ok : n:nat -> Lemma (fib 1 1 n = fibonacci n)
let fib_is_ok n = fib_is_ok_helper n 0
