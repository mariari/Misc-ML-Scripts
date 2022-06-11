

type 'b lambda_bb = {
  un_lambda_bb :
    'a. ('b -> 'a) -> (string -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a;
}
type 'b lambda_one = {
  un_one :
    'a.
      ('b -> 'a) ->
      ('b lambda_bb -> 'a) -> ('b lambda_bb -> 'b lambda_bb -> 'a) -> 'a;
}
val var : 'a -> 'a lambda_bb
val abs : string -> 'a lambda_bb -> 'a lambda_bb
val app : 'a lambda_bb -> 'a lambda_bb -> 'a lambda_bb
val test_lam : string lambda_bb
val test_lam_nameless : int lambda_bb


module Eval : sig
  type t = int lambda_bb
  val shift : 'a -> by:'b -> int
end
