module Noalloc

open FStar.ST

val write_value : #a:Type0 -> #rel:Preorder.preorder a
                -> h : Heap.heap
                -> r : Heap.mref a rel{h `Heap.contains` r} -> a
                -> Heap.heap

let write_value #a #rel heap ref v =
  let h1 = Heap.upd_tot heap ref v in
  h1


val no_modify : (#rel:Preorder.preorder int) -> int
              -> ST int (fun h -> True)
                     (fun h0 _ h1 ->
                       Heap.equal_dom h0 h1
                       )
let no_modify #rel (x : int) =
  let heap  = get () in
  let r, h1 = Heap.alloc rel heap x true in
  let h2    = write_value h1 r 3 in
  Heap.lemma_upd_equals_upd_tot_for_contained_refs h1 r 3;
  let heap2 = Heap.free_mm h2 r in
  gst_put heap2;
  3
