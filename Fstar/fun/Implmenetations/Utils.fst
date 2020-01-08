module Utils

module Map = FStar.OrdMap

type addr = int

module LT = FStar.List.Tot.Base

val map_accum_l (#acc #a #b : Type) :
                (acc -> a -> (acc * b))
                -> acc
                -> xs : list a
                -> Tot (acc * (ys : list b))
                      (decreases %[xs])
let rec map_accum_l #_ #_ #_ f s = function
  | []     -> s, []
  | x :: xs ->
    let s, y  = f s x in
    let s, ys = map_accum_l f s xs in
    s, y :: ys

val map_accum_length_same (#acc #a #b : Type)
  : f  : (acc -> a -> (acc * b))
  -> ac : acc
  -> xs : list a
  -> Lemma (requires True)
          (ensures (let (_, zs) = map_accum_l f ac xs in
                   LT.length zs = LT.length xs))
           (decreases %[xs])
           [SMTPat (map_accum_l f ac xs)]
let rec map_accum_length_same #_ #_ #_ f acc = function
  | []     -> ()
  | hd :: tl ->
    let s, _ = f acc hd in
    map_accum_length_same f s tl

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

val list_drop_only : n : nat
                   -> xs : list 'a{LT.length xs >= n}
                   -> ys : list 'a{LT.length ys = LT.length xs - n}
let rec list_drop_only num xs =
  match num with
  | 0 -> xs
  | n ->
    match xs with
    | x :: xs -> list_drop_only (num - 1) xs


val split_only_acc : n    : nat
                   -> #a  : Type
                   -> xs  : list 'a{LT.length xs >= n}
                   -> acc : list 'a
                   -> ( zs : list 'a {LT.length zs = n + LT.length acc}
                      * ys : list 'a{LT.length ys = LT.length xs - n})
let rec split_only_acc num #a xs acc =
  match num with
  | 0 -> List.Tot.Properties.rev_length acc;
        (LT.rev acc, xs)
  | n ->
    match xs with
    | x :: xs -> split_only_acc (num - 1) #a xs (x :: acc)

val split_only : n : nat
               -> (#a : Type)
               -> xs : list a{LT.length xs >= n}
               -> ( zs : list a{LT.length zs = n}
                 * ys : list a{LT.length ys = LT.length xs - n})
let split_only n #a xs = split_only_acc n #a xs []


val zip_same_len
  : xs : list 'a
  -> ys : list 'b{LT.length ys = LT.length xs}
  -> zs : list ('a * 'b){LT.length zs = LT.length ys}
let rec zip_same_len xs ys =
  match xs, ys with
  | x :: xs, y :: ys -> (x, y) :: zip_same_len xs ys
  | []    , []     -> []
