open Types
open Exceptions
open Inference
open Options
open Affichage


let ajout_inf = ref [];;




(*Renvoie true si x apparaît dans term*)
let rec appear v t = 
  match t with
  | Var (v',_,_)   -> v = v'
  | T (TTuple (a,b))
  | T (TFun (a,b)) -> appear v a || appear v b
  | T TRef l
  | T TList l      -> appear v l
  | _              -> false
      
(*Effectue la substitution sigma(term) = term[new_x/x] *)
let rec replace (x, new_x) = function 
  | Var (v,t1,b1)         -> if v = x then (if t1 <> new_x then ajout_inf := (t1, new_x) :: !ajout_inf; new_x) else Var (v,replace (x,new_x) t1,b1)
  | T (TFun (arg, corps)) -> T (TFun ((replace (x, new_x) arg), replace (x, new_x) corps))
  | T (TRef r)            -> T (TRef (replace (x, new_x) r))
  | T (TTuple (a,b))      -> T (TTuple (replace (x, new_x) a, replace (x, new_x) b))  
  | T TList l             -> T (TList (replace (x,new_x) l))
  | T (TExn t)            -> T (TExn (replace (x,new_x) t))
  | a                     -> a



let rec replace_prime (maxi, mini) = function
    | Prime c when c = maxi -> mini
    | Var (v,t1,b1)         -> Var (v,replace_prime (maxi,mini) t1,b1)
    | T (TFun (arg, corps)) -> T (TFun ((replace_prime (maxi, mini) arg), replace_prime (maxi, mini) corps))
    | T (TRef r)            -> T (TRef (replace_prime (maxi, mini) r))
    | T (TTuple (a,b))      -> T (TTuple (replace_prime (maxi, mini) a, replace_prime (maxi, mini) b))  
    | T (TList l)             -> T (TList (replace_prime (maxi,mini) l))
    | T (TExn t)              -> T (TExn (replace_prime (maxi, mini) t))
    | a                     -> a



let compt = ref 0;;

let rec type_fixed = function
  | T TInt
  | T TBool
  | T TUnit          -> true
  | T (TFun (a,b)) 
  | T (TTuple (a,b)) -> type_fixed a && type_fixed b
  | T TRef t 
  | T TList t
  | T TExn t 
  | Var (_,t,_)      -> type_fixed t
  | Prime a          -> !compt > compt_max
  | _                -> false


let rec find_var v = function
  | Var (v',t,b)     when v = v' -> Var (v,t,b)
  | T TFun(a,_)      when appear v a -> find_var v a
  | T TFun(_,a)      when appear v a -> find_var v a
  | T TRef a         when appear v a -> find_var v a 
  | T (TTuple (a,_)) when appear v a -> find_var v a
  | T (TTuple (_,a)) when appear v a -> find_var v a
  | T TList a        when appear v a -> find_var v a
  | T TExn a         when appear v a -> find_var v a
  | _ -> None


let rec find_var_list v = function
  | [] -> None
  | (a, _) :: _ when appear v a -> find_var v a
  | (_, a) :: _ when appear v a -> find_var v a
  | _ :: pb' -> find_var_list v pb'


let rec fusion_type t1 t2 = 
  match t1, t2 with
  | t, None
  | None, t                                -> t, []
  | t, Prime a
  | Prime a, t                             -> t, [Prime a, t]
  | T (TFun (a1, b1)), T (TFun (a2,b2))    -> let t1,l1 = fusion_type a1 a2 and t2, l2 = fusion_type b1 b2 in T (TFun(t1, t2)), l1 @ l2
  | T (TTuple(a1, b1)), T (TTuple (a2,b2)) -> let t1,l1 = fusion_type a1 a2 and t2, l2 = fusion_type b1 b2 in T (TTuple(t1, t2)), l1 @ l2
  | T (TRef t1), T (TRef t2)               -> let t, l = fusion_type t1 t2 in T (TRef t), l
  | T (TList t1), T (TList t2)             -> let t, l = fusion_type t1 t2 in T (TList t), l
  | T (TExn t1), T (TExn t2)               -> let t, l = fusion_type t1 t2 in T (TExn t), l
  | t1, t2                                 -> t1, [(t1,t2)]


let rec no_var = function
  | Var (_,t,_) -> no_var t
  | T (TFun (a,b)) -> T (TFun (no_var a, no_var b))
  | T (TTuple (a,b)) -> T (TTuple (no_var a, no_var b))
  | T (TRef a) -> T (TRef (no_var a))
  | T (TList a) -> T (TList (no_var a))
  | T (TExn a) -> T (TExn (no_var a))
  | a -> a


let rec occ_in t1 = function
  | t when t = t1 -> true
  | T (TTuple (a,b))
  | T (TFun (a,b)) -> occ_in t1 a || occ_in t1 b
  | T (TRef a)
  | T (TExn a)
  | T (TList a) -> occ_in t1 a
  | _ -> false

let rec occurs_in t1 = function
  | Var (_,t,_) -> occurs_in t1 t
  | T (TTuple (a,b))
  | T (TFun (a,b)) -> occ_in t1 a || occ_in t1 b
  | T (TRef a)
  | T (TExn a)
  | T (TList a) -> occ_in t1 a
  | _ -> false



(*Implémente l'unification de deux termes*)
let rec unify pb = if !Options.showinf && !compt > compt_max then print_string "out of compt\n"; 
  let update_inf pb = if !Options.showinf then (print_string "AJOUT : ";print_prob !ajout_inf); let pb' = !ajout_inf @ pb in ajout_inf := []; unify pb' in
  if !ajout_inf <> [] then update_inf pb else (incr compt; if !Options.showinf then print_prob pb;
  match pb with
  | [] -> ()
  | x :: pb' -> begin
    match x with
    | T a, T b when a = b -> unify pb'
    | Var (v, t, b), _ when type_fixed t || !compt >= compt_max -> retype v (no_var t); (unify (List.map (fun (t1,t2) -> replace_prime (prime_var v !correspondance, t) (replace (v, t) t1), replace_prime (prime_var v !correspondance, t) (replace (v, t) t2)) pb')) 
    | _, Var (v, t, b) when type_fixed t || !compt >= compt_max -> retype v (no_var t); (unify (List.map (fun (t1,t2) -> replace_prime (prime_var v !correspondance, t) (replace (v, t) t1), replace_prime (prime_var v !correspondance, t) (replace (v, t) t2)) pb'))
    | None, Var (v,t,b)
    | Var (v,t,b), None -> unify (pb'  @ ([(Var (v,t,b), find_var_list v pb')]))
    | _, None -> unify pb'
    | None, _ -> unify pb'
    | Prime a, Prime b -> occ_prime.(min a b) <- occ_prime.(min a b) + occ_prime.(max a b); occ_prime.(max a b) <- 0;
                          unify (List.map (fun (t1,t2) -> (replace_prime (max a b, Prime (min a b)) t1), replace_prime (max a b, Prime (min a b)) t2) pb')
    | t, Prime a when occurs_in (Prime a) t -> raise (Impossible_occ (Prime a, no_var t))
    | Prime a, t when occurs_in (Prime a) t -> raise (Impossible_occ (Prime a, no_var t))
    | Var (_,t,_), Prime a  
    | Prime a, Var (_,t,_)
    | t, Prime a
    | Prime a, t -> occ_prime.(a) <- 0; unify (List.map (fun (t1,t2) -> (replace_prime (a, t) t1, replace_prime (a, t) t2)) pb')
    | T (TTuple (a,b)), T (TTuple (c,d)) -> unify ((a,c) :: (b,d) :: pb') 
    | T (TFun (a,b)), T (TFun (c,d)) -> unify ((a,c) :: (b,d) :: pb')
    | T (TRef a), T (TRef b) -> unify ((a,b) :: pb')
    | T (TList a), T (TList b) -> unify ((a,b) :: pb')
    | T a, T b -> raise (TypeError (T a, T b))
    | Var (v1,t1,b1), Var (v2,t2,b2) when v1 = v2 -> let t,l = fusion_type t1 t2 in unify (pb' @ l @ [(Var(v1, t, b1 || b2), None)])
    | Var (v1,t1,b1), Var (v2,t2,b2) -> unify (pb' @ [(Var(v1, Var (v2,t2,b2), b1 || b2), None); Var (v2, Var (v1,t1,b1), b1 || b2), None])
    | t, Var (v,t1,b1)
    | Var (v,t1,b1), t -> let t2, l = fusion_type t1 t in unify ((Var (v, t2, b1), None) :: pb' @ l)
  end)


