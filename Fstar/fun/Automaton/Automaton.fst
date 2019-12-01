module Automaton

module List = FStar.List.Tot
open FStar.Tactics
open FStar.Tactics.Derived

(**** four move machine *)

type move =
  | Left
  | Right
  | Up
  | Down

type move_set = list move

(* Useful for the turning machine, when we look in a direction *)
type direction = move
val direction_to_mod_4 : move -> n : nat{n < 4}
let direction_to_mod_4 = function
  | Left  -> 0
  | Right -> 2
  | Up    -> 1
  | Down  -> 3


type base_state = {
  spot : int * int
}


// | 0,0 | 0,1 | 0,2  | 0,3  | 0,4   | 0,5 |
// |-----+-----+------+------+-------+-----|
// | 1,0 |     |      |      |       |     |
// | 2,0 |     |      |  up  |       |     |
// | 3,0 |     | left | here | right |     |
// | 4,0 |     |      | down |       |     |
// | 5,0 |     |      |      |       |     |

val app_inst : base_state -> move -> base_state
let app_inst {spot} move =
  let (x,y) = spot in
  { spot =
      match move with
      | Left  -> (x    , y - 1)
      | Right -> (x    , y + 1)
      | Up    -> (x - 1, y)
      | Down  -> (x + 1, y)
  }


val run : base_state -> move_set -> base_state
let run s xs = List.fold_left app_inst s xs

let left  = List.count Left
let right = List.count Right
let up    = List.count Up
let down  = List.count Down


val run_n_times : n : nat -> base_state -> move_set -> base_state
let rec run_n_times n s xs =
  match n with
  | 0 -> s
  | n -> run_n_times (n - 1) (run s xs) xs

let default_spot = {spot = (0,0)}

let spot_to_int {spot} = let (x,y) = spot in abs x + abs y


let test = run default_spot [Left; Right; Up; Down]


(* TODO ∷ learn to abstract this logic into 1 call, via tactitcs? *)

val y_relationship : xs : move_set
                   → s  : base_state
                   → Lemma (snd s.spot + (right xs - left xs) == (snd (run s xs).spot))
let rec y_relationship xs s =
  match xs with
  | []         -> ()
  | Up    :: xs -> y_relationship xs (app_inst s Up)
  | Down  :: xs -> y_relationship xs (app_inst s Down)
  | Left  :: xs -> y_relationship xs (app_inst s Left)
  | Right :: xs -> y_relationship xs (app_inst s Right)


val x_relationship : xs : move_set
                   → s  : base_state
                   → Lemma (fst s.spot + (down xs - up xs) == (fst (run s xs).spot))
let rec x_relationship xs s =
  match xs with
  | []         -> ()
  | Up    :: xs -> x_relationship xs (app_inst s Up)
  | Down  :: xs -> x_relationship xs (app_inst s Down)
  | Left  :: xs -> x_relationship xs (app_inst s Left)
  | Right :: xs -> x_relationship xs (app_inst s Right)

val proof_bounded : xs  : move_set
                  -> spot : base_state
                  → Lemma (requires left xs == right xs /\ up xs == down xs)
                          (ensures spot == run spot xs)
let proof_bounded xs state =
  x_relationship xs state;
  y_relationship xs state


val proof_bounded_n : xs   : move_set
                    → spot : base_state
                    → n    : nat
                    → Lemma (requires left xs == right xs /\ up xs == down xs)
                            (ensures spot == run_n_times n spot xs)
let rec proof_bounded_n xs spot n =
  match n with
  | 0 -> ()
  | n ->
    proof_bounded xs spot;
    proof_bounded_n xs spot (n - 1)

val proof_unbounded : xs   : move_set
                    → spot : base_state
                    → Lemma (requires left xs <> right xs \/ up xs <> down xs)
                            (ensures spot <> run spot xs)
let proof_unbounded xs s =
  match xs with
  | x :: xs ->
    y_relationship xs (app_inst s x);
    x_relationship xs (app_inst s x)

val proof_unbounded_n : xs   : move_set
                      → spot : base_state
                      → n    : nat
                      → Lemma (requires left xs <> right xs \/ up xs <> down xs)
                              (ensures exists m. spot_to_int (run_n_times m spot xs) > n)
let rec proof_unbounded_n xs spot n =
  // TODO!
  admit ()



(**** turn machine  *)

type turn =
  | Turn_left
  | Foward
  | Turn_right

type turn_set = list turn
