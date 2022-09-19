(** fundamental point *)
(* OO does not distinguish between these two signature wise *)

(* sig foo : ∀ x,y :> bag. x -> y -> x
 * let foo bag1 bag2 =
 *   bag1.append(bag2)
 *
 * sig foo' : ∀ x,y :> bag. x -> y -> bag
 * let foo' bag1 bag2 =
 *   let newBag = new bag in
 *   newBag.append(bag1).append(bag2) *)

(* http://okmij.org/ftp/Computation/Subtyping/ *)

let rec repeat n f acc =
  match n with
  | 0 -> acc
  | n -> repeat (pred n) f (f acc)

(* This signature is 100% optional, taken from the OCaml REPL *)
class type ['a] signature =
  object
    (* Excluding this actually changes the late binding behavior! *)
    method private create : ('a * int) list -> 'a signature
    method to_list : ('a * int) list
    method insert  : 'a -> 'a signature
    method lookup  : 'a -> ('a * int) option
    method append  : 'a signature -> 'a signature
    (* Notice how these signatures are the same! *)
    method foo      : 'a signature -> 'a signature
    method foo_fast : 'a signature -> 'a signature
  end


(* A bag is implemented as a list of tuples, the simplest implementation *)
class ['a] bag list : ['a] signature = object(s)

  method private state : ('a * int) list = list

  method to_list = s#state

  (* hack to get around self types not escaping *)
  (* You normally don't make this kind of constructor in normal
     mutative or even pure OO code, so we will not use it for our foo
     example. *)
  method private create = new bag

  method insert input =
    let rec add_to_counter = function
      | x :: xs when fst x = input ->
         (fst x, succ (snd x)) :: xs
      | x :: xs ->
         x :: add_to_counter xs
      | [] ->
         [input, 1]
    in s#create (add_to_counter s#to_list)

  method lookup to_find =
    let rec lookup = function
      | x :: xs when fst x = to_find -> Some x
      | x :: xs                      -> lookup xs
      | []                           -> None
    in lookup s#state

  method append (bag2 : 'a bag) =
    let f current_bag (element, times) =
      repeat times (fun bag -> bag#insert element) current_bag
    in
    List.fold_left f (s#create s#state) bag2#to_list

  (* an un-optimized version of foo' *)
  method foo (bag2 : 'a bag) =
    s#append bag2

  (* an optimized version of foo *)
  (* Ocaml makes breaking a bit harder *)
  method foo_fast (bag2 : 'a bag) =
    let bag3 = new bag [] in
    (bag3#append (s#create s#state))#append bag2
end

(* we must only have 1 element in the bag *)
let rec remove_excess = function
  | x :: xs ->
     (fst x, 1) :: remove_excess xs
  | [] -> []

(* A set is a bag, but, we only have 1 element, not n  *)
class ['a] set list : ['a] signature = object(s)
  inherit ['a] bag (remove_excess list)
  (* Again ugly hack to get around escaping *)
  method private create = new set
end

(* we can see the interface agrees with the implementation *)
(* We've done regression testing on them, they are the same! *)
let bag_foo =
  let bag = new bag ["a", 3; "b", 4] in
  (* Always the same no matter what you give it *)
  (bag#foo_fast bag)#to_list, (bag#foo bag)#to_list

(* uh oh! *)
let set_foo =
  let set = new set ["a", 3; "b", 4] in
  (* These are never equal, except in the empty case *)
  (* Note the signature is the same but our interface changed, and
     regression testing can't find it *)
  (set#foo_fast set)#to_list, (set#foo set)#to_list


(* Output *)
(* val bag_foo : (string * int) list * (string * int) list =
 *   ([("a", 6); ("b", 8)], [("a", 6); ("b", 8)])
 * val set_foo : (string * int) list * (string * int) list =
 *   ([("a", 2); ("b", 2)], [("a", 1); ("b", 1)]) *)


(* appendex A unneeded *)

(* A set is a bag, but  *)
class ['a] set_worse list = object(s)
  inherit ['a] bag (remove_excess list)
  method insert input =
    let rec add_to_counter = function
      | x :: xs when fst x = input ->
         (fst x, snd x) :: xs
      | x :: xs ->
         x :: add_to_counter xs
      | [] ->
         [input, 1]
    in new set_worse (add_to_counter s#to_list)

  (* Again ugly hack to get around escaping *)
  method private construct = new set_worse
end
