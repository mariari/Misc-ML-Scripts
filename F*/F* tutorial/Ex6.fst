module Ex6
#set-options "--z3rlimit_factor 2"

open Ex1

open FStar.All

val sorted : ('a → 'a → bool) -> list 'a → bool
let rec sorted f = function
  | []         → true
  | [x]        → true
  | x :: y :: xs → f x y && sorted f (y :: xs)

val count : #t:eqtype -> t -> list t -> Tot nat
let rec count #t (x:t) (l:list t) = match l with
  | hd :: tl -> if hd = x then 1 + count x tl else count x tl
  | [] -> 0

val append_count: #t:eqtype -> l:list t -> m:list t
               -> Lemma (requires True)
                        (ensures (forall x. count x (append l m) = (count x l + count x m)))
                        [SMTPat (append l m)]
let rec append_count #t l m = match l with
  | [] -> ()
  | hd::tl -> append_count tl m

val partition : ('a -> bool) -> list 'a -> (list 'a * list 'a)
let partition f xs =
  List.Tot.fold_right (fun x (ins, out) →
                         if f x
                         then (x :: ins, out)
                         else (ins    , x :: out))
                      xs
                      ([],[])


(* sometimes finicky with z3 if you don't include the set options *)
val partition_lemma : #a:eqtype
                    → f : (a -> Tot bool)
                    -> l : list a
                    -> Lemma (requires True)
                            (ensures (List.Tot.length (fst (partition f l))
                                     + List.Tot.length (snd (partition f l)) = List.Tot.length l
                            /\ (forall x. mem x (fst (partition f l)) ==> f x)
                            /\ (forall x. mem x (snd (partition f l)) ==> not (f x))
                            /\ (forall x. mem x l = (mem x (fst (partition f l))
                                             || mem x (snd (partition f l))))
                            /\ (forall x. count x l = (count x (fst (partition f l))
                                               + count x (snd (partition f l))))))
                                [SMTPat (partition f l)]
let rec partition_lemma #a f l = match l with
  | [] -> ()
  | hd :: tl -> partition_lemma f tl



(** [total_order] - the function f must give rise to an POSET *)
type total_order (#a:eqtype) (f: (a -> a -> bool)) =
    (forall a. f a a)                                           (* reflexivity   *)
    /\ (forall a1 a2. (f a1 a2 /\ a1 <> a2)  <==> not (f a2 a1))  (* anti-symmetry *)
    /\ (forall a1 a2 a3. f a1 a2 /\ f a2 a3 ==> f a1 a3)        (* transitivity  *)


type permutation (#a:eqtype) (l : list a) (m : list a) =
    (forall i. mem   i l = mem   i m)
  /\ (forall i. count i l = count i m)


val sorted_concat_lemma : #a : eqtype
                        → f  : (a -> a -> bool)
                        → l1 : list a{sorted f l1}
                        → l2 : list a{sorted f l2}
                        → pivot : a
                        → Lemma (requires (total_order f
                                         /\ (forall y. mem y l1 ==> not (f pivot y))
                                         /\ (forall y. mem y l2 ==> f pivot y)))
                                 (ensures (sorted f (append l1 (pivot :: l2))))
                                 [SMTPat (sorted f (append l1 (pivot :: l2)))]
let rec sorted_concat_lemma #a f l1 l2 pivot =
    match l1 with
    | []     -> ()
    | _ :: tl -> sorted_concat_lemma f tl l2 pivot

val sort : #a : eqtype
         → f  : (a -> a -> bool){total_order f}
         → l  : list a
         → Tot (m : list a{sorted f m /\ (permutation l m)})
               (decreases (List.Tot.length l))
let rec sort #a f = function
  | [] -> []
  | pivot :: tl ->
    let hi, lo = partition (f pivot) tl in
    let m      = append (sort f lo) (pivot :: sort f hi) in
    assert (forall i. mem   i (pivot :: sort f hi) = mem   i (append [pivot] (sort f hi)));
    assert (forall i. count i (pivot :: sort f hi) = count i (append [pivot] (sort f hi)));
    m
