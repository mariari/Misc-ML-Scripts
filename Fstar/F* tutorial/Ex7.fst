module Ex7

type ty =
  | TBool  : ty
  | TArrow : tin:ty → tout:ty → ty

type var = int

type exp =
  | EVar   : v:var -> exp
  | EApp   : fn:exp -> arg:exp -> exp
  | EAbs   : v:var -> vty:ty -> body:exp -> exp
  | ETrue  : exp
  | EFalse : exp
  | EIf    : test:exp -> btrue:exp -> bfalse:exp -> exp


let stlc_app_id_to_true = EApp (EAbs 0 TBool (EVar 0)) ETrue

val is_value : exp -> Tot bool
let is_value = function
  | EAbs _ _ _
  | ETrue
  | EFalse     -> true
  | _          -> false


val subst : var -> exp -> exp -> exp
let rec subst x e e' =
  match e' with
  | EVar x' -> if x = x' then e else e'
  | EAbs x' t e1 ->
      EAbs x' t (if x = x' then e1 else (subst x e e1))
  | EApp e1 e2 -> EApp (subst x e e1) (subst x e e2)
  | ETrue -> ETrue
  | EFalse -> EFalse
  | EIf e1 e2 e3 -> EIf (subst x e e1) (subst x e e2) (subst x e e3)

val step : exp -> Tot (option exp)
  let rec step e =
  match e with
  | EApp e1 e2 ->
      if is_value e1 then
        if is_value e2 then
          match e1 with
          | EAbs x t e' -> Some (subst x e2 e')
          | _           -> None
        else
          match (step e2) with
          | Some e2' -> Some (EApp e1 e2')
          | None     -> None
      else
        (match (step e1) with
        | Some e1' -> Some (EApp e1' e2)
        | None     -> None)
  | EIf e1 e2 e3 ->
      if is_value e1 then
        match e1 with
        | ETrue   -> Some e2
        | EFalse  -> Some e3
        | EAbs _ _ _ -> None
      else
        (match (step e1) with
        | Some e1' -> Some (EIf e1' e2 e3)
        | None     -> None)
  | _ -> None

let _ = assert (step (EApp (EAbs 0 TBool (EVar 0)) ETrue) = Some ETrue)
let _ = assert (step (EApp ETrue ETrue) = None)

type test = i : int{i > 0}

type env = int -> Tot (option ty)

val empty : env
let empty = fun _ -> None

val extend : env -> int -> ty -> Tot env
let extend g x t = fun x' -> if x = x' then Some t else g x'

val typing : env -> exp -> Tot (option ty)
let rec typing g e =
  match e with
  | EVar x -> g x
  | EAbs x t e1 ->
      (match typing (extend g x t) e1 with
      | Some t' -> Some (TArrow t t')
      | None    -> None)
  | EApp e1 e2 ->
      (match typing g e1, typing g e2 with
      | Some (TArrow t11 t12), Some t2 -> if t11 = t2 then Some t12 else None
      | _                    , _       -> None)
  | ETrue  -> Some TBool
  | EFalse -> Some TBool
  | EIf e1 e2 e3 ->
      (match typing g e1, typing g e2, typing g e3 with
      | Some TBool, Some t2, Some t3 -> if t2 = t3 then Some t2 else None
      | _         , _      , _       -> None)

val progress : e:exp → Lemma (requires (Some? (typing empty e)))
                             (ensures (is_value e \/ (Some? (step e))))
let rec progress = function
  | EApp e1 e2   -> progress e1; progress e2
  | EIf e1 e2 e3 -> progress e1; progress e2; progress e3
  | _            -> ()


val appears_free_in : x:int -> e:exp -> Tot bool
let rec appears_free_in x e =
  match e with
  | EVar y -> x = y
  | EApp e1 e2 -> appears_free_in x e1 || appears_free_in x e2
  | EAbs y _ e1 -> x <> y && appears_free_in x e1
  | EIf e1 e2 e3 ->
      appears_free_in x e1 || appears_free_in x e2 || appears_free_in x e3
  | ETrue
  | EFalse -> false

val free_in_context : x:int -> e:exp -> g:env -> Lemma
      (requires (Some? (typing g e)))
      (ensures (appears_free_in x e ==> Some? (g x)))
let rec free_in_context x e g =
  match e with
  | EVar _
  | ETrue
  | EFalse -> ()
  | EAbs y t e1 -> free_in_context x e1 (extend g y t)
  | EApp e1 e2 -> free_in_context x e1 g; free_in_context x e2 g
  | EIf e1 e2 e3 -> free_in_context x e1 g;
                    free_in_context x e2 g; free_in_context x e3 g

val typable_empty_closed : x:int -> e:exp -> Lemma
      (requires (Some? (typing empty e)))
      (ensures (not(appears_free_in x e)))
      [SMTPat (appears_free_in x e)]
let typable_empty_closed x e = free_in_context x e empty

let equal (g1:env) (g2:env) : Type0 = (forall (x:int). g1 x = g2 x)
let equalE (e:exp) (g1:env) (g2:env) : Type0 = (forall (x:int). appears_free_in x e ==> g1 x = g2 x)

val context_invariance : e:exp -> g:env -> g':env -> Lemma
  (requires (equalE e g g'))
  (ensures (typing g e == typing g' e))
let rec context_invariance e g g' =
  match e with
  | EAbs x t e1 ->
     context_invariance e1 (extend g x t) (extend g' x t)
  | EApp e1 e2 ->
     context_invariance e1 g g';
     context_invariance e2 g g'
  | EIf e1 e2 e3 ->
     context_invariance e1 g g';
     context_invariance e2 g g';
     context_invariance e3 g g'
  | _ -> ()

val typing_extensional : g:env -> g':env -> e:exp -> Lemma
  (requires (equal g g'))
  (ensures (typing g e == typing g' e))
let typing_extensional g g' e = context_invariance e g g'


val substitution_preserves_typing : x:int -> e:exp -> v:exp -> g:env -> Lemma
  (requires (Some? (typing empty v)
            /\  Some? (typing (extend g x (Some?.v (typing empty v))) e)))
  (ensures (Some? (typing empty v)
           /\ typing g (subst x v e)
           == typing (extend g x (Some?.v (typing empty v))) e))
let rec substitution_preserves_typing x e v g =
  let Some t_x = typing empty v in
  let gx = extend g x t_x in
  match e with
  | ETrue -> ()
  | EFalse -> ()
  | EVar y ->
     if x=y
     then context_invariance v empty g (* uses lemma typable_empty_closed *)
     else context_invariance e gx g

  | EApp e1 e2 ->
     substitution_preserves_typing x e1 v g;
     substitution_preserves_typing x e2 v g

  | EIf e1 e2 e3 ->
     substitution_preserves_typing x e1 v g;
     substitution_preserves_typing x e2 v g;
     substitution_preserves_typing x e3 v g

  | EAbs y t_y e1 ->
     let gxy = extend gx y t_y in
     let gy = extend g y t_y in
     if x=y
     then typing_extensional gxy gy e1
     else
       (let gyx = extend gy x t_x in
        typing_extensional gxy gyx e1;
        substitution_preserves_typing x e1 v gy)

val preservation : e:exp -> Lemma
  (requires (Some? (typing empty e) /\ Some? (step e)))
  (ensures (Some? (step e) /\
            typing empty (Some?.v (step e)) == typing empty e))
let rec preservation e =
  match e with
  | EApp e1 e2 ->
     if is_value e1
     then (if is_value e2
           then let EAbs x _ ebody = e1 in
                substitution_preserves_typing x ebody e2 empty
           else preservation e2)
     else preservation e1

  | EIf e1 _ _ ->
      if not (is_value e1) then preservation e1


val typed_step : e:exp{Some? (typing empty e) /\ not(is_value e)}
               -> Tot (e':exp{typing empty e' = typing empty e})
let typed_step e =
  progress e;
  preservation e;
  Some?.v (step e)
