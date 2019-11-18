module Utils

// maybe change to generating OCaml maps!
module Map = FStar.OrdMap
module Set = FStar.OrdSet

type addr = int

unopteq type heap (a : Type) : Type = {
  size    : int;
  free    : l : (list int){Cons? l};
  maps_to : Map.ordmap int a (<=)
}

val h_initial (#a : Type) : (heap a)
let h_initial #a = {
  size = 0;
  free = [1];
  maps_to = Map.empty
}

val h_alloc (#a : Type) : heap a -> a -> heap a * int
let h_alloc #a {size; free; maps_to} v =
  match free with
  | [x] -> ({
    size    = size + 1;
    free    = [x + 1];
    maps_to = Map.update x v maps_to
    }, x)
  | x :: xs -> ({
    size    = size + 1;
    free    = xs;
    maps_to = Map.update x v maps_to
  }, x)

val h_update (#a : Type) : heap a -> int -> a -> heap a
let h_update #a heap key val' =
  {heap with maps_to = Map.update key val'  heap.maps_to}

val h_free (#a : Type) : heap a -> int -> heap a
let h_free #a {size; free; maps_to} key = {
  size    = size - 1;
  free    = key :: free;
  maps_to = Map.remove key maps_to
}

// For some reason as_list is private, but we can make it with fold
private val set_as_list (#a:eqtype) (#acc:Type) (#f:Set.cmp a) : Set.ordset a f -> list a
private let set_as_list #_ #_ #_ s =
  Set.fold (fun acc x -> x :: acc) [] s

val h_addresses (#a : Type) : heap a -> list int
let h_addresses #a {size; free; maps_to} =
   set_as_list #int #int (Map.dom maps_to)

val h_lookup (#a : Type) : heap a -> int -> Prims.Tot (option a)
let h_lookup #_ {maps_to} val' = Map.select val' maps_to

val h_size (#a : Type) : heap a -> int
let h_size #a = Mkheap?.size
