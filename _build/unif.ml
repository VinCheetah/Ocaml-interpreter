open Types
           
exception Not_unifyable (*à "raise" si le problème n'a pas de solution*)

(*Renvoie true si x apparaît dans term*)
let rec appear motif t = 
  match t with
  | Var v -> motif = v
  | T (TFun (arg,corps)) -> appear motif arg || appear motif corps
  | T (TRef r) -> appear motif r
  | T (TTuple (a,b)) -> appear motif a || appear motif b
  | _ -> false
      
(*Effectue la substitution sigma(term) = term[new_x/x] *)
let rec replace (x, new_x) term =
  match term with
  | Var v -> if Var v = x then new_x else Var v
  | T (TFun (arg,corps)) -> T (TFun ((if arg = x then new_x else arg), replace (x, new_x) corps))
  | T (TRef r) -> T (TRef (if x = r then new_x else replace (x, new_x) r))
  | T (TTuple (a,b)) as tuple -> if tuple = x then new_x else T (TTuple (replace (x, new_x) a, replace (x, new_x) b))  
  | a -> a



(*Implémente l'unification de deux termes*)
let rec unify pb =
  (* let rec aux l1 l2 =
    match l1, l2 with
    | [], [] -> []
    | x1 :: l1', x2 :: l2' -> (x1,x2) :: (aux l1' l2')
    | _ -> failwith "Different length"
  in *)


  match pb with
  | [] -> []
  | x :: pb' -> begin
    match x with
    | None, _ 
    | _, None -> unify pb'
    | T (TTuple (a,b)), T (TTuple (c,d)) -> unify ((a,c)::(b,d)::pb') 
  (*| T TFun (arg, corps), T TFun (arg', corps') -> unify ((arg, arg'))     
    | Fun (s,l), Fun (s',l') -> if s = s' then unify ((aux l l') @ pb')
                                else raise Not_unifyable*)
    | T a, T b when a != b-> raise Not_unifyable
    | T a, T b -> unify pb'
    | Var v, Var v' when v = v' -> unify pb'
    | Var v, t
    | t, Var v -> if appear v t then raise Not_unifyable else (Var v, t) :: (unify (List.map (fun (t1,t2) -> (replace (Var v, t) t1), replace (Var v, t) t2) pb'))
                              end


