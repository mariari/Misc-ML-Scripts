open Core

(* from LOL *)
let word_trigger str =
  let index = ref 0 in
  let count = ref 0 in
  if str = "" then
    stage (fun input -> (count := !count + 1; !count))
  else
    let capacity () = if !index = String.length str then begin
                          count := !count + 1;
                          index := 0
                        end in
    let f x = if x = String.get str !index
              then index := !index + 1
              else index := 0 in
    stage @@
    fun input ->
      String.iter input (fun x -> f x; capacity ());
      capacity ();
      !count

let look_out_bob = word_trigger "bob"

(*
# unstage look_out_bob "bob";;
- : int = 1
# unstage look_out_bob "bo";;
- : int = 1
# unstage look_out_bob "b";;
- : int = 2
*)

(* Written by an OCAML friend *)

module type Word_trigger = sig
  type t (* hide the internal representation of t *)
  val create : string -> t
  val find : t -> string -> int
end

module Word : Word_trigger = struct
  type t = string -> int

  let create str =
    let index = ref 0 in
    let count = ref 0 in
    if str = "" then
      (fun input -> (count := !count + 1; !count))
    else
      let capacity () =
        if !index = String.length str
        then (count := !count + 1 ; index := 0) in
      let f x =
        if x = String.get str !index
        then index := !index + 1
        else index := 0 in
      fun input ->
        String.iter input (fun x -> f x; capacity ());
        capacity ();
        !count
  let find f s = f s
end


(* state/reader monad anyone!?!? *)
let bob = Word.create "bob"
