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
let app_inst {spot = (x,y)} move =
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


val run_n_times : nat -> base_state -> move_set -> base_state
let rec run_n_times n s xs =
  match n with
  | 0 -> s
  | n -> run_n_times (n - 1) (run s xs) xs

let default_spot = {spot = (0,0)}

let spot_to_nat {spot} = let (x,y) = spot in abs x + abs y


let test = run_n_times 10 default_spot [Left; Up]


(* TODO ∷ learn to abstract this logic into 1 call, via tactics? *)

// let generate_logic f : Tac unit =
//   let xs = intro () in
//   let s  = intro () in
//   let test =
//   `(match (`#xs) with
//     | [] -> ()
//     | Up    :: xs -> (`@f) xs (app_inst (`#s) Up)
//     | Down  :: xs -> (`@f) xs (app_inst (`#s) Down)
//     | Left  :: xs -> (`@f) xs (app_inst (`#s) Left)
//     | Right :: xs -> (`@f) xs (app_inst (`#s) Right)
//   )
//   in
//   apply test

let generate_logic' f (xs : move_set) (s : base_state) : Tac unit =
  let test =
  `(match (`@xs) with
    | [] -> ()
    | Up    :: ys -> (`@f) ys (app_inst (`@s) Up)
    | Down  :: ys -> (`@f) ys (app_inst (`@s) Down)
    | Left  :: ys -> (`@f) ys (app_inst (`@s) Left)
    | Right :: ys -> (`@f) ys (app_inst (`@s) Right)
  )
  in
  apply test

  // let te = 
  // match xs with
  // | []     -> exact (`())
  // | y :: ys ->
  //     let te = (`((`@f) (`@ys) (app_inst (`@s) (`@y)))) in
  //     exact te


// val y_relationship : xs : move_set
//                    → s  : base_state
//                    → Lemma (snd s.spot + (right xs - left xs) == (snd (run s xs).spot))
// let rec y_relationship xs s = _ by (generate_logic' y_relationship xs s)

val y_relationship : xs : move_set
                   → s  : base_state
                   → Lemma (snd s.spot + (right xs - left xs) == (snd (run s xs).spot))
let rec y_relationship xs s =
  match xs with
  | []      -> ()
  | x  :: xs -> y_relationship xs (app_inst s x)

val x_relationship : xs : move_set
                   → s  : base_state
                   → Lemma (fst s.spot + (down xs - up xs) == (fst (run s xs).spot))
let rec x_relationship xs s =
  match xs with
  | []      -> ()
  | x  :: xs -> x_relationship xs (app_inst s x)

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



(**** Types about unboundedness *)

type inequal_move xs = left xs <> right xs \/ up xs <> down xs

type increasing xs spot = spot_to_nat (run spot xs) > spot_to_nat spot

(**** Proof about unboundedness *)
val proof_unbounded : xs   : move_set
                    → spot : base_state
                    → Lemma (requires inequal_move xs)
                            (ensures spot <> run spot xs)
let proof_unbounded xs s =
  match xs with
  | x :: xs ->
    y_relationship xs (app_inst s x);
    // x_relationship xs (app_inst s x);
    _ by ()

val spot_at_zero_relation : xs : move_set{inequal_move xs}
                         → Lemma (spot_to_nat (run default_spot xs) > 0)
let spot_at_zero_relation xs =
  proof_unbounded xs default_spot

val spot_to_nat_relation : xs : move_set
                         → Lemma (requires inequal_move xs)
                                 (ensures spot_to_nat (run default_spot xs) > spot_to_nat default_spot)
let spot_to_nat_relation xs =
  spot_at_zero_relation xs


(***** God analysis is annoying! *)
// (x,y) increasing ==> spot_to_nat (x + n, y + m)  > spot_to_nat (x,y)
// |x + n| > |x| \/ |y + m| > |y|

val spot_abs_relation : xs : move_set
                      -> spot : base_state
                      -> Lemma (increasing xs spot ==>
                                          (let {spot = (x, y)}     = spot in
                                           let {spot = (x_n, y_m)} = run spot xs in
                                           abs x_n > abs x \/ abs y_m > abs y))
let spot_abs_relation xs spot = ()

(***** all that is commented out is bad and probably wrong
       apply analysis to get good *)
// val continuous_relation : xs   : move_set
//                         → spot : base_state
//                         → Lemma (requires increasing xs spot)
//                                 (ensures
//                                  ( let run_spot_nat     = spot_to_nat (run spot xs) in
//                                    let spot_nat         = spot_to_nat spot in
//                                    let run_run_spot_nat = spot_to_nat (run (run spot xs) xs) in
//                                    let diff_1           = run_spot_nat - spot_nat in
//                                    let diff_2           = run_run_spot_nat - run_spot_nat in
//                                     diff_1 = diff_2
//                                    ))
// (100, -20) - 50 + 70
// let continuous_relation xs spot =
//   if (left xs <> right xs || up xs <> down xs)
//   then begin
//     proof_unbounded xs spot;
//     proof_unbounded xs (run spot xs);
//     assert (spot_to_nat (run spot xs) - spot_to_nat spot > 0);
//     assert (spot_to_nat (run (run spot xs) xs) - spot_to_nat (run spot xs) > 0);
//     admit ()
//   end else
//     proof_bounded xs spot


// val spot_relation : xs : move_set
//                   -> spot : base_state
//                   -> n    : nat{n > spot_to_nat spot}
//                   → Lemma (requires inequal_move xs)
//                           (ensures spot_to_nat (run_n_times n spot xs) >= spot_to_nat spot)
// let rec spot_relation xs spot n =
//   match n with
//   | 1 -> ()
//   | n ->
//     proof_unbounded xs spot;
//     spot_relation xs spot (n - 1)

// val spot_to_nat_relation_n
//   : xs : move_set
//   → n  : nat{n > 0}
//   → Lemma (requires inequal_move xs)
//           (ensures spot_to_nat (run_n_times n default_spot xs)
//                    > spot_to_nat (run_n_times (n - 1) default_spot xs))

// let rec spot_to_nat_relation_n xs n =
//   match n with
//   | 1 → spot_at_zero_relation xs
//   | n → spot_to_nat_relation_n xs (n - 1)


val proof_unbounded_n : xs   : move_set
                      → spot : base_state
                      → n    : nat
                      → Lemma (requires inequal_move xs)
                              (ensures exists m. spot_to_nat (run_n_times m spot xs) > n)
let rec proof_unbounded_n xs spot n =
  // TODO!
  admit ()



(**** turn machine  *)

type turn =
  | Turn_left
  | Foward
  | Turn_right

type turn_set = list turn
