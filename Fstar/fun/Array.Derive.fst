module Array.Derive

let preorder = FStar.Preorder.preorder

module F = FStar.FunctionalExtensionality

// Taken from monotonic_heap
private noeq type heap_rec = {
  next_addr: pos;
  memory   : F.restricted_t pos (fun (x:pos)
             -> option (a:Type0 & rel:(option (preorder a)) & b:bool & a))
                      //type, preorder, mm flag, and value
}

let heap = h:heap_rec{(forall (n:nat). n >= h.next_addr ==> None? (h.memory n))}


// came from the monotonic_heap module
private noeq type monref' (a:Type0) (rel:FStar.Preorder.preorder a) : Type0 = {
  addr: (x: nat { x > 0 } );
  init: a;
  mm:   bool;  //manually managed flag
}


[@ assume_strictly_positive]
val monref (a:Type0) (rel:preorder a) :Type0
let monref a rel = monref' a rel

val addr_of: #a:Type0 -> #rel:preorder a -> monref a rel -> GTot pos
let addr_of #a #rel r = r.addr

val is_mm: #a:Type0 -> #rel:FStar.Preorder.preorder a -> monref a rel -> GTot bool
let is_mm #a #rel r = r.mm

val heap_contains: #a:Type0 -> #rel:preorder a -> heap -> monref a rel -> Type0
let heap_contains #a #rel h r =
  Some? (h.memory r.addr) /\
  (let Some (| a1, pre_opt, mm, _ |) = h.memory r.addr in
   a == a1 /\ Some? pre_opt /\ Some?.v pre_opt == rel /\ mm = r.mm)  //using `===` here, since otherwise typechecker fails with a and a1 being different types, why?


// Taken from ST
let contains_pred
  (#a:Type0)
  (#rel:preorder a)
  (r:monref a rel) =
  fun h -> h `heap_contains` r


type heap_predicate = heap -> Type0

val sel_tot:
  #a:Type0 -> #rel:preorder a -> h:heap -> r:monref a rel{h `heap_contains` r} -> Tot a
let sel_tot #a #rel h r =
  let Some (| _, _, _, x |) = h.memory r.addr in
  x

val sel: #a:Type0 -> #rel:preorder a -> heap -> monref a rel -> GTot a
let sel #a #rel h r =
  if FStar.StrongExcludedMiddle.strong_excluded_middle (h `heap_contains` r) then
    sel_tot #a h r
  else r.init

let heap_rel (h1:heap) (h2:heap) =
  forall (a:Type0) (rel:preorder a) (r:monref a rel).
    h1 `heap_contains` r ==> (h2 `heap_contains` r /\ rel (sel h1 r) (sel h2 r))


let stable (p:heap_predicate) =
  forall (h1:heap) (h2:heap). (p h1 /\ heap_rel h1 h2) ==> p h2


abstract let witnessed (p:heap_predicate{stable p}) : Type0 =
  FStar.Monotonic.Witnessed.witnessed heap_rel p

type mref (a:Type0) (rel:FStar.Preorder.preorder a) =
  r:monref a rel{is_mm r = false /\ witnessed (contains_pred r)}


let trivial_rel (a:Type0) : Preorder.relation a = fun x y -> True

let trivial_preorder (a:Type0) : Preorder.preorder a = trivial_rel a

type ref (a:Type0) = mref a (trivial_preorder a)

let array a = ref (Seq.seq a)

// let us make our own

val as_ref (#a:Type0) (arr:array a) : GTot (ref (Seq.seq a))
let as_ref #_ arr = arr

let addr_of_arr (#a:Type0) (arr:array a) : GTot nat = addr_of (as_ref arr)


// ST write, very cute, but very janky in how we exert things
// abstract let write (#a:Type) (#rel:preorder a) (r:mref a rel) (v:a)
//   : ST unit
//     (fun h -> rel (sel h r) v)
//     (fun h0 x h1 -> rel (sel h0 r) v /\ h0 `contains` r /\
//                  modifies (Set.singleton (addr_of r)) h0 h1 /\ equal_dom h0 h1 /\
//                  sel h1 r == v)
//   = let h0 = gst_get () in
//     gst_recall (contains_pred r);
//     let h1 = upd_tot h0 r v in
//     Heap.lemma_distinct_addrs_distinct_preorders ();
//     Heap.lemma_distinct_addrs_distinct_mm ();
//     Heap.lemma_upd_equals_upd_tot_for_contained_refs h0 r v;
//     gst_put h1
