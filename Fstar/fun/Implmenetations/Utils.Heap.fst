module Utils.Heap

// maybe change to generating OCaml maps!
// OrdMap is an OrdSet: which is just an
// abstraction over a sorted list!
module Map = FStar.OrdMap
module Set = FStar.OrdSet


// free is the next free value label
unopteq type t (a : Type) : Type = {
  size    : int;
  free    : l : (list int){Cons? l};
  maps_to : Map.ordmap int a (<=)
}

val initial (#a : Type) : (t a)
let initial #a = {
  size = 0;
  free = [1];
  maps_to = Map.empty
}


val alloc (#a : Type) : t a -> a -> t a * int
let alloc #a {size; free; maps_to} v =
  let curr, next_frees =
    match free with
    | [x]    -> x, [x + 1]
    | x :: xs -> x, xs
  in ({ size    = size + 1;
        free    = next_frees;
        maps_to = Map.update curr v maps_to
      }, curr)

val update (#a : Type) : t a -> int -> a -> t a
let update #a heap key val' =
  {heap with maps_to = Map.update key val'  heap.maps_to}

val h_free (#a : Type) : t a -> int -> t a
let h_free #a {size; free; maps_to} key = {
  size    = size - 1;
  free    = key :: free;
  maps_to = Map.remove key maps_to
}

// For some reason as_list is private, but we can make it with fold
private val set_as_list (#a:eqtype) (#acc:Type) (#f:Set.cmp a) : Set.ordset a f -> list a
private let set_as_list #_ #_ #_ =
  Set.fold (fun acc x -> x :: acc) []

val addresses (#a : Type) : t a -> list int
let addresses #a {size; free; maps_to} =
   set_as_list #int #int (Map.dom maps_to)

val lookup (#a : Type) : t a -> int -> Prims.Tot (option a)
let lookup #_ {maps_to} val' = Map.select val' maps_to

val size (#a : Type) : t a -> int
let size #a = Mkt?.size

