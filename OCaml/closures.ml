
(* from LOL *)
let word_trigger str =
  let index = ref 0 in
  let count = ref 0 in
  let capacity = fun () -> if !index = String.length str
                           then begin
                              count := !count + 1;
                              index := 0
                            end in
  let f x = if x = String.get str !index
            then index := !index + 1
            else index := 0 in
  fun input ->
    String.iter (fun x -> x |> f |> capacity) input;
    capacity () ;
    !count

let look_out_bob = word_trigger "bob"

(*
look_out_bob "bob";;
- : int = 1
# look_out_bob "bo";;
- : int = 1
# look_out_bob "b";;
- : int = 2
*)



(* Written by an OCAML friend *)

module Word_trigger : sig
  type t (* hide the internal representation of t *)
  val create : string -> t
  val find : t -> string -> int
end = struct
  type t = string -> int

  let create str =
    let index = ref 0 in
    let count = ref 0 in
    let capacity () =
      if !index = String.length str
      then (count := !count + 1 ; index := 0) in
    let f x =
      if x = String.get str !index
      then index := !index + 1
      else index := 0 in
    fun input ->
      String.iter (fun x -> x |> f |> capacity) input ;
      capacity () ;
      !count

  let find f s = f s
end


(* state/reader monad anyone!?!? *)
let bob = Word_trigger.create "bob";;
