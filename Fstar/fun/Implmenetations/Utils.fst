module Utils

module Map = FStar.OrdMap

type addr = int

val map_accum_l (#acc #a #b : Type) :
                (acc -> a -> (acc * b))
                -> acc
                -> xs : list a
                -> Tot (acc * list b)
                      (decreases %[xs])
let rec map_accum_l #_ #_ #_ f s = function
  | []     -> s, []
  | x :: xs ->
    let s, y  = f s x in
    let s, ys = map_accum_l f s xs in
    s, y :: ys


let test = map_accum_l (fun x acc -> x + acc, x) 0 [1;2;3]

val list_to_map : #a   : eqtype
                -> #b  : Type
                -> xs  : list (a * b)
                -> cmp : Map.cmp a
                -> Map.ordmap a b cmp
let list_to_map #_ #_ xs cmp =
  List.Tot.Base.fold_right (fun (k,v) acc -> Map.update k v acc)
                           xs
                           Map.empty


val typeOf (#a:Type u#a) : a -> Type u#a
let typeOf #a _ = a

val list_to_map_t : #a    : eqtype
                  -> #b   : Type
                  -> #cmp : Map.cmp a
                  -> xs   : list (a * b)
                  -> Map.ordmap a b cmp
let list_to_map_t #a #b #cmp xs  =
  List.Tot.Base.fold_right (fun (k,v) acc -> Map.update k v acc)
                           xs
                           Map.empty

val list_drop : nat -> xs : list 'a -> Tot (list 'a) (decreases %[xs])
let rec list_drop num xs =
  match num with
  | 0 -> xs
  | n ->
    match xs with
    | x :: xs -> list_drop (num - 1) xs
    | []     -> []

module LT = FStar.List.Tot.Base

val list_drop_only : n : nat
                   -> xs : list 'a{LT.length xs >= n}
                   -> ys : list 'a{LT.length ys = LT.length xs - n}
let rec list_drop_only num xs =
  match num with
  | 0 -> xs
  | n ->
    match xs with
    | x :: xs -> list_drop_only (num - 1) xs

