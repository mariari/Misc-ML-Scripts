module Ex8

type vector (a:Type) : nat -> Type =
   | Nil :  vector a 0
   | Cons : hd:a -> #n:nat -> tl:vector a n -> vector a (n + 1)

//val reverse_vector: #a:Type -> #n:nat -> vector a n -> Tot (vector a n)
// is same as
val reverse_vector: #n:nat -> vector 'a n -> Tot (vector 'a n)

