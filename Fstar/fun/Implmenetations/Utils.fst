module Utils

// maybe change to generating OCaml maps!
module Map = FStar.Map

type addr = int

unopteq type heap (a : Type) : Type = {
  size    : int;
  free    : l : (list int){Cons? l};
  maps_to : Map.t int (option a)
}

val h_initial (#a : Type) : (heap a)
let h_initial #a = {
  size = 0;
  free = [1];
  maps_to = Map.const None
}

let test = Map.contains ((h_initial #int).maps_to) 2


val h_alloc (#a : Type) : heap a -> a -> heap a * int
let h_alloc #a {size; free; maps_to} v =
  match free with
  | [x] -> ({
    size    = size + 1;
    free    = [x + 1];
    maps_to = Map.upd maps_to x (Some v)
    }, x)
  | x :: xs -> ({
    size    = size + 1;
    free    = xs;
    maps_to = Map.upd maps_to x (Some v)
  }, x)

val h_update (#a : Type) : heap a -> int -> a -> heap a
let h_update #a heap key val' =
  {heap with maps_to = Map.upd heap.maps_to key (Some val')}

val h_free (#a : Type) : heap a -> int -> heap a
let h_free #a {size; free; maps_to} key = {
  size    = size - 1;
  free    = key :: free;
  maps_to = Map.upd maps_to key None
}
