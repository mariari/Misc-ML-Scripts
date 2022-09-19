module Token

open FStar
open FStar.All

module Map = FStar.OrdMap
module MapProp = FStar.OrdMapProps


(*******************************************************************)
(* Boiler plate taken from previous code *)
(*******************************************************************)

type total_order (a:eqtype) (f: (a -> a -> Tot bool)) =
   (forall a1 a2. (f a1 a2 /\ f a2 a1)  ==> a1 = a2)  (* anti-symmetry *)
 /\ (forall a1 a2 a3. f a1 a2 /\ f a2 a3 ==> f a1 a3)  (* transitivity  *)
 /\ (forall a1 a2. f a1 a2 \/ f a2 a1)                (* totality      *)

let string_cmp' s1 s2 =  String.compare s1 s2 <= 0

(* The F* defn just calls down to OCaml, since we know comparison in OCaml is total
 * just admit it
 *)
val string_cmp_total : unit -> Lemma (total_order string string_cmp')
let string_cmp_total () = admit ()

// hack function so, the data structure doesn't forget the proof!
val string_cmp : Map.cmp string
let string_cmp = string_cmp_total (); string_cmp'

(*******************************************************************)
(* Boiler plate over      *)
(*** Begin Type Definitions *)
(*******************************************************************)

type address = a : string {String.length a == 36}

type accounts = Map.ordmap address nat string_cmp

val add_account_values_acc : accounts → nat -> nat
let add_account_values_acc accounts n  =
 MapProp.fold (fun _key (v : nat) (acc : nat) -> v + acc) accounts n

// we have to specify they are nats :(
val add_account_values : accounts -> nat
let add_account_values accounts =
 MapProp.fold (fun _key (v : nat) (acc : nat) -> v + acc) accounts 0

unopteq type storage = {
  total_supply : nat;
  accounts : a : accounts { total_supply = add_account_values a };
}

val empty_storage : storage
let empty_storage = {
  total_supply = 0;
  accounts = Map.empty;
}

unopteq type token = {
  storage : storage;
  version : nat;  // version of the token contract
  name    : string;
  symbol  : Char.char;
  owner   : address // it seems that tokens can have owners for minting
}

unopteq type tx_transfer = {
  from_account    : address;
  to_account      : address;
  transfer_amount : nat
}

unopteq type tx_mint = {
  mint_amount     : nat;
  mint_to_account : address
}

unopteq type tx_burn = {
  burn_amount       : nat;
  burn_from_account : address
}

unopteq type tx_data =
  | Transfer of tx_transfer
  | Mint     of tx_mint
  | Burn     of tx_burn

unopteq type tx = {
  tx_data               : tx_data;
  tx_authroized_account : address
}

(*******************************************************************)
(* End Type Definitions *)
(**** Begin Functions On Accounts *)
(*******************************************************************)


val has_n : accounts -> address -> nat -> bool
let has_n accounts add to_transfer =
  match Map.select add accounts with
  | Some n -> to_transfer <= n
  | None   -> false

val account_sub : acc : accounts
                -> add : address
                -> num : nat {has_n acc add num}
                -> accounts
let account_sub accounts add number =
  match Map.select add accounts with
  | Some balance -> Map.update add (balance - number) accounts


// No feedback given, so don't know next move :(
val transfer_sub : acc : accounts
                 -> add : address
                 -> num : nat
                 -> Lemma
                  (requires (has_n acc add num))
                  (ensures (  add_account_values acc - num
                           == add_account_values (account_sub acc add num)))
let transfer_sub acc add num =
  match Map.select add acc with
  | Some balance ->
    let remaining : nat = balance - num in
    admit ()

val account_add : acc : accounts
                -> add : address
                -> num : nat
                -> accounts
let account_add acc add num =
  match Map.select add acc with
  | Some b' -> Map.update add (b' + num) acc
  | None    -> Map.update add num        acc

(*******************************************************************)
(**** Experimental Proofs on Transfer *)
(*******************************************************************)

val transfer_add_lemma : acc : accounts
                       -> add : address
                       -> num : nat
                       -> Lemma
                       (ensures
                         (let i =
                           match Map.select add acc with
                           | None   -> 0
                           | Some v -> v
                          in i + num = Some?.v (Map.select add (account_add acc add num))))
let transfer_add_lemma acc add num = ()

val transfer_add_unaffect : acc : accounts
                          -> add : address
                          -> num : nat
                          -> Lemma
                          (ensures
                            (forall x. Map.contains x acc /\ x <> add
                              ==> Map.select x acc = Map.select x (account_add acc add num)))
let transfer_add_unaffect acc add num = ()


val transfer_same_when_remove : acc : accounts
                              -> add : address
                              -> num : nat
                              -> Lemma
                              (ensures
                                (let new_account = account_add acc add num in
                                    add_account_values (Map.remove add acc)
                                 == add_account_values (Map.remove add new_account)))
let transfer_same_when_remove acc add num =
  let new_account = account_add acc add num in
  assert (Map.equal (Map.remove add acc) (Map.remove add new_account))

// Useful stepping stone to the real answer!
// sadly admitted for now
val transfer_acc_behavior : acc : accounts
                          -> add : address
                          -> Lemma
                            (ensures
                              (let i =
                                match Map.select add acc with
                                | None   -> 0
                                | Some v -> v
                                in add_account_values_acc (Map.remove add acc) i
                                  == add_account_values acc))
let transfer_acc_behavior acc add =
  admit ()

// No feedback given, so don't know next move :(
val transfer_add : acc : accounts
                 -> add : address
                 -> num : nat
                 -> Lemma
                  (ensures ( add_account_values acc + num
                           == add_account_values (account_add acc add num)))
let transfer_add acc add num =
  admit ();
  transfer_add_lemma acc add num;
  transfer_same_when_remove acc add num;
  transfer_add_unaffect acc add num

(*******************************************************************)
(**** Failed Experimental Proofs Over *)
(*******************************************************************)


val transfer_acc : acc     : accounts
                -> add_from : address
                -> add_to   : address
                -> num      : nat {has_n acc add_from num}
                -> accounts
let transfer_acc accounts add_from add_to number =
  let new_accounts = account_sub accounts add_from number in
  account_add new_accounts add_to number

val transfer_maintains_supply
  : acc      : accounts
  -> add_from : address
  -> add_to   : address
  -> num      : nat
  -> Lemma
    (requires (has_n acc add_from num))
    (ensures (add_account_values acc
              == add_account_values (transfer_acc acc add_from add_to num)))
let transfer_maintains_supply acc add_from add_to num =
  transfer_sub acc add_from num;
  transfer_add (account_sub acc add_from num) add_to num

val transfer_stor
  : stor     : storage
  -> add_from : address
  -> add_to   : address
  -> num      : nat {has_n stor.accounts add_from num}
  -> storage
let transfer_stor stor add_from add_to num =
  let new_acc = account_add (account_sub stor.accounts add_from num) add_to num in
  transfer_maintains_supply stor.accounts add_from add_to num;
  { total_supply = stor.total_supply;
    accounts     = new_acc
  }


(*******************************************************************)
(* End Type Definitions *)
(**** Begin Validations On Tokens *)
(*******************************************************************)


val valid_transfer : token -> tx -> bool
let valid_transfer token tx =
  match tx.tx_data with
  | Transfer {from_account; transfer_amount} ->
    has_n token.storage.accounts from_account transfer_amount
    && tx.tx_authroized_account = from_account
  | Mint _ | Burn _ ->
    false

val valid_mint : token -> tx -> bool
let valid_mint token tx =
  match tx.tx_data with
  | Mint mint           -> token.owner = tx.tx_authroized_account
  | Transfer _ | Burn _ -> false

val valid_burn : token -> tx -> bool
let valid_burn token tx =
  match tx.tx_data with
  | Burn {burn_from_account; burn_amount} ->
    has_n token.storage.accounts burn_from_account burn_amount
    && tx.tx_authroized_account = burn_from_account
  | Transfer _ | Mint _ ->
    false


(*******************************************************************)
(* End validations on tokens *)
(**** Begin Functions On Tokens *)
(*******************************************************************)

val token_transaction : (token -> tx -> bool) -> Type0
let token_transaction f =
  tok : token -> tx : tx { f tok tx } -> token

val transfer : token_transaction valid_transfer
let transfer token transaction =
  match transaction.tx_data with
  | Transfer {from_account; to_account; transfer_amount} ->
    { token
      with storage = transfer_stor token.storage
                                   from_account
                                   to_account
                                   transfer_amount
    }

val mint : token_transaction valid_mint
let mint token transaction =
  match transaction.tx_data with
  | Mint {mint_amount; mint_to_account} ->
    transfer_add token.storage.accounts mint_to_account mint_amount;
    { token
      with storage = {
        total_supply = token.storage.total_supply + mint_amount;
        accounts     = account_add token.storage.accounts mint_to_account mint_amount
      }}

val burn : token_transaction valid_burn
let burn token transaction =
  match transaction.tx_data with
  | Burn {burn_from_account; burn_amount} ->
    transfer_sub token.storage.accounts burn_from_account burn_amount;
    { token
      with storage = {
        total_supply = token.storage.total_supply - burn_amount;
        accounts     = account_sub token.storage.accounts burn_from_account burn_amount
      }}

type transaction_error =
  | Not_enough_funds
  | Not_same_account
  | Not_owner_token
  | Not_enough_tokens

val execute_transaction : token -> tx -> c_or transaction_error token
let execute_transaction token tx =
  match tx.tx_data with
  | Transfer _ ->
    if valid_transfer token tx
    then Right (transfer token tx)
    else Left Not_enough_funds // todo determine what the error is
  | Mint _ ->
    if valid_mint token tx
    then Right (mint token tx)
    else Left Not_owner_token
  | Burn _ ->
    if valid_burn token tx
    then Right (burn token tx)
    else Left Not_enough_tokens

val valid_transfer_transaction
  : tok : token
  -> tx  : tx
  -> Lemma (requires (valid_transfer tok tx))
          (ensures (
            let Right new_tok = execute_transaction tok tx in
            tok.storage.total_supply == new_tok.storage.total_supply))
let valid_transfer_transaction tok tx = ()


val valid_mint_transaction
  : tok : token
  -> tx  : tx
  -> Lemma (requires (valid_mint tok tx))
          (ensures (
            let Right new_tok = execute_transaction tok tx in
            let Mint {mint_amount} = tx.tx_data in
            tok.storage.total_supply + mint_amount == new_tok.storage.total_supply))
let valid_mint_transaction tok tx = ()

val valid_burn_transaction
  : tok : token
  -> tx  : tx
  -> Lemma (requires (valid_burn tok tx))
          (ensures (
            let Right new_tok = execute_transaction tok tx in
            let Burn {burn_amount} = tx.tx_data in
            tok.storage.total_supply - burn_amount == new_tok.storage.total_supply))
let valid_burn_transaction tok tx = ()

let isLeft = function
  | Left _  -> true
  | Right _ -> false

val non_valid_transaction
  : tok : token
  -> tx  : tx
  -> Lemma (requires (not (valid_burn tok tx)
                   /\ not (valid_mint tok tx)
                   /\ not (valid_transfer tok tx)))
          (ensures (isLeft (execute_transaction tok tx)))
let non_valid_transaction tok tx = ()

