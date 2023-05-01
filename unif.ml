type types =
  | TInt
  | TBool
  | TFun of t*t
  | TUnit
  | TRef of t
  | TTuple of t*t
  | TList of t
  | TExcep


type t =
  | Var of motif
  | Fun of string*t list
  | T of types 
  | None

type problem = (t * t) list
           
exception Not_unifyable (*à "raise" si le problème n'a pas de solution*)

(*Renvoie true si x apparaît dans term*)
let rec appear motif t = 
  match t with
  | Var v -> motif = v
  | Fun (s, l) -> begin
    match l with
    | [] -> false
    | t' :: l' -> (appear motif t') || (appear motif (Fun (s, l'))) 
  end ;;
      
(*Effectue la substitution sigma(term) = term[new_x/x] *)
let rec replace ((x, new_x) : var * t) (term : t) : t =
  match term with
  | Var v -> if v = x then new_x else Var v
  | Fun (s,l) -> Fun (s,List.map (fun t' -> replace (x, new_x) t') l)



(*Implémente l'unification de deux termes*)
let rec unify (pb : problem) =
  let rec aux l1 l2 =
    match l1, l2 with
    | [], [] -> []
    | x1 :: l1', x2 :: l2' -> (x1,x2) :: (aux l1' l2')
    | _ -> failwith "Different length"
  in


  match pb with
  | [] -> []
  | x :: pb' -> begin
    match x with
    | None, _ 
    | _, None -> unify pb'
    | T a, T b -> if a != b then raise Not_unifyable else unify pb'    
    | Fun (s,l), Fun (s',l') -> if s = s' then unify ((aux l l') @ pb')
                                else raise Not_unifyable
    | Var v, Var v' when v = v' -> unify pb'
    | Var v, t
    | t, Var v -> if appear v t then raise Not_unifyable else (v, t) :: (unify (List.map (fun (t1,t2) -> (replace (v, t) t1), replace (v, t) t2) pb'))


