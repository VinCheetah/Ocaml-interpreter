open Types
open Inference
exception Not_unifyable (*à "raise" si le problème n'a pas de solution*)
open Options


let ajout_inf = ref [];;




(*Renvoie true si x apparaît dans term*)
let rec appear motif t = 
  match t with
  | Var (v,_,_) -> motif = v
  | T (TFun (arg,corps)) -> appear motif arg || appear motif corps
  | T (TRef r) -> appear motif r
  | T (TTuple (a,b)) -> appear motif a || appear motif b
  | _ -> false
      
(*Effectue la substitution sigma(term) = term[new_x/x] *)
let rec replace (x, new_x) term =
  match term with
  | Var (v,t1,b1) -> if v = x then (ajout_inf := (t1, new_x) :: !ajout_inf; new_x) else Var (v,replace (x,new_x) t1,b1)
  | T (TFun (arg, corps)) -> T (TFun ((replace (x, new_x) arg), replace (x, new_x) corps))
  | T (TRef r) -> T (TRef (replace (x, new_x) r))
  | T (TTuple (a,b)) -> T (TTuple (replace (x, new_x) a, replace (x, new_x) b))  
  | a -> a

let compt = ref 0;;

let rec type_fixed = function
  | T TInt
  | T TBool
  | T TUnit -> true
  | T TRef t -> type_fixed t
  | T (TFun (a,b)) 
  | T (TTuple (a,b))-> type_fixed a && type_fixed b
  | Var (_,t,_) -> type_fixed t
  | _ -> false


let rec find_var v' = function
  | Var (v,t,b) when v = v' -> Var (v,t,b)
  | T TFun(a,_) when appear v' a -> find_var v' a
  | T TFun(_,a) when appear v' a -> find_var v' a
  | T TRef a when appear v' a -> find_var v' a
  | T (TTuple (a,_)) when appear v' a -> find_var v' a
  | T (TTuple (_,a)) when appear v' a -> find_var v' a
  | _ -> None


let rec find_var_list v = function
  | [] -> None
  | (a, _) :: _ when appear v a -> find_var v a
  | (_, a) :: _ when appear v a -> find_var v a
  | _ :: pb' -> find_var_list v pb'


let rec fusion_type t1 t2 = 
  match t1, t2 with
  | t, None
  | None, t -> t, []
  | T (TFun (a1, b1)), T (TFun (a2,b2)) -> let t1,l1 = fusion_type a1 a2 and t2, l2 = fusion_type b1 b2 in T (TFun(t1, t2)), l1 @ l2
  | T (TTuple(a1, b1)), T (TTuple (a2,b2)) -> let t1,l1 = fusion_type a1 a2 and t2, l2 = fusion_type b1 b2 in T (TTuple(t1, t2)), l1 @ l2
  | t1, t2 -> t1, [(t1,t2)]






(*Implémente l'unification de deux termes*)
let rec unify pb =
  let update_inf pb = if !Options.showinf then (print_string "AJOUT : ";print_prob !ajout_inf); let pb' = !ajout_inf @ pb in ajout_inf := []; unify pb' in
  if !ajout_inf <> [] then update_inf pb else (incr compt; if !Options.showinf then print_prob pb;
  match pb with
  | [] -> []
  | x :: pb' -> begin
    match x with
    | Var (v, t, b), _ when type_fixed t || !compt >= 5000 -> if appear v t then raise Not_unifyable else (unify (List.map (fun (t1,t2) -> (replace (v, t) t1), replace (v, t) t2) pb')) @ (if true then [(Var (v,None,false), t)] else [])
    | _, Var (v, t, b) when type_fixed t || !compt >= 5000 -> if appear v t then raise Not_unifyable else (unify (List.map (fun (t1,t2) -> (replace (v, t) t1), replace (v, t) t2) pb')) @ (if true then [(Var (v,None,false), t)] else [])
    | T (TTuple (a,b)), T (TTuple (c,d)) -> unify ((a,c)::(b,d)::pb') 
    | T (TFun (a,b)), T (TFun (c,d)) -> unify ((a,c)::(b,d)::pb')
    | T (TRef a), T (TRef b) -> unify ((a,b)::pb')
    | T a, T b when a != b-> raise Not_unifyable
    | T a, T b -> unify pb'
    | Var (v,t1,b1), Var (v2,t2,b2) when v = v2-> let t,l = fusion_type t1 t2 in unify (pb' @ l @ [(Var(v, t, b1 || b2), None)])
    | Var (v,t1,b1), Var (v2,t2,b2) -> unify (pb' @ [(Var(v, Var (v2,t2,b2), b1 || b2), None); Var (v2, Var (v,t1,b1), b1 || b2), None])
    | Var (v,t,b), None -> unify (pb'  @ (if true then [(Var (v,t,b), find_var_list v pb')] else []))
    | t, Var (v,t1,b1)
    | Var (v,t1,b1), t -> let t2, l = fusion_type t1 t in unify ((Var (v, t2, b1), None) :: pb' @ l)
    | _, None -> unify pb'
    | None, _ -> unify pb'
  end)


