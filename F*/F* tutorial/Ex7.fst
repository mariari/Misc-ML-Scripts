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
        | _       -> None
      else
        (match (step e1) with
        | Some e1' -> Some (EIf e1' e2 e3)
        | None     -> None)
  | _ -> None
