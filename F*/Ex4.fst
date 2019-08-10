module Ex4


open Ex1


val fold_left : ('a -> 'b -> 'a)
              → 'a
              -> l : list 'b
              -> Tot 'a (decreases l)
let rec fold_left f b xs =
  match xs with
  | []      → b
  | hd :: tl → fold_left f (f b hd) tl

let consFlip xs x = x :: xs

val fold_left_cons_is_reverse_empty : l   : list 'a
                                    -> Lemma (fold_left consFlip [] l == reverse l)

let rec fold_left_cons_is_reverse_empty = function
  | []     → ()
  | x :: xs →
  snoc_cons xs x;
  fold_left_cons_is_reverse_empty xs;
  // can't go much from here Ι need the (hd :: b), but b is not bound!
  admit ()



val append_assoc : l1:list 'a -> l2:list 'a -> l3: list 'a ->
  Lemma (append l1 (append l2 l3) == append (append l1 l2) l3)
let rec append_assoc l1 l2 l3 =
  match l1 with
  | [] -> ()
  | h1 :: t1 -> append_assoc t1 l2 l3

val fold_left_cons_is_reverse : l   : list 'a
                              → acc : list 'a
                              → Lemma (fold_left consFlip acc l == append (reverse l) acc)
let rec fold_left_cons_is_reverse l1 l2 =
  match l1 with
  | [] → ()
  | h1 :: t1 →
    // (1) [append (append (reverse t1) [h1]) t2]
    //     == append (reverse t1) (append [h1] t2)]
    append_assoc (reverse t1) [h1] l2;
    // (2) [fold_left consFlip (h1 :: l2) t1 == append (reverse t1) (h1 :: l2)]
    fold_left_cons_is_reverse t1 (h1 :: l2)
    (*
     * append (reverse l1) l2
     * =by definition  = append (append (reverse t1) [hd]) l2 // of Reverse
     * =by 1/assoc     = append (reverse t1) (append [hd] l2)
     * =by definition  = append (reverse t1) (h1 :: l2)       // of Append
     * =by 2/induction = fold_left flipCons (h1 :: l2) l1
     * =by definition  = fold_left flipCons l1 l2
     *)
