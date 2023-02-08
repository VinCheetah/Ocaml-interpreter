(* d�finition des diff�rents types : tout est � reprendre et �tendre *)


(* définition du type pour les opérateurs *)
type op =
  |Le
  |Ge
  |L
  |G
  |Eq
  |Ne
  |And
  |Or

 (* définition du type pour les expressions*) 
type expr =
  Const of int
| Var of string 
| Add of expr*expr
| Mul of expr*expr
| Min of expr*expr
| Op of op*expr*expr
| If of expr*expr*expr
| PrInt of expr
| Let of string*expr*expr 


 (* définition du type pour les valeurs*) 
type valeur = 
  VInt of int
  |VBool of bool    
  
(* définition du type des environnements*)
type env = (string*valeur)  list

let empty_env = []

(* fonction qui permet de modifier un environnement (ajouter une variable ou l'écraser et la rajouter avec une nouvelle valeur)*)
let modifier_env s valeur env = match (List.mem  ((s,valeur)) env) with
  |false -> (( (s,valeur))::env)
  |true -> begin let rec aux_parcours env  = match env with 
              |[] -> failwith "cas impossible"
              |c::q -> let (s1,v1) = c in 
                if (s1 == s) then ((s,valeur)::q) 
                else c::(aux_parcours q)
              in aux_parcours env
              end
(* s�mantique op�rationnelle � grands pas *)


(* permet de récupérer la valeur d'une variable dans un environnement*)
let rec trouver_env s env = match env with
  |[] -> failwith "Variable non présente"
  |(x,v)::q -> if (x=s) then  v   else  trouver_env s q 

(* évaluation à grands pas *)
let rec eval e env = match e with
  |Var k -> trouver_env k env
  |Const k -> VInt k
  |Add(e1,e2) -> let VInt k1 = (eval e1 env) and VInt k2 = (eval e2 env) in VInt (k1 + k2)
  |Mul(e1,e2) -> let VInt k1 = (eval e1 env) and VInt k2 = (eval e2 env) in VInt (k1 * k2)
  |Min(e1,e2) -> let VInt k1 = (eval e1 env) and VInt k2 = (eval e2 env) in VInt (k1 - k2)
  |If(c1,e1,e2) -> begin match c1 with 
                    |Op(op,e3,e4) -> if (evalop op e3 e4 env) then (eval e1 env) else (eval e2 env)
                    |_ -> failwith "Pas de conditions dans if" 
                    end
  |PrInt(e1) -> begin let v1 = (eval e1 env ) in match v1 with
                    |VInt k1 -> print_int k1 ; print_newline () ; v1;
                    |_ -> failwith "Non traité"
                  end 
              
  |Let(s,e1,e2) -> let v1 = (eval e1 env) in (eval e2 (modifier_env s v1 env))

  | _ -> failwith "Le cas Op n'est pas traité"

(* évaluation des expressions contenants des opérateurs booléens *)
and  evalop op c1 c2 env = match op with
  |Le -> let VInt k1 = (eval c1 env) and VInt k2 = (eval c2 env) in (k1 <= k2) 
  |Ge -> let VInt k1 = (eval c1 env) and VInt k2 = (eval c2 env) in (k1 >= k2)
  |L -> let VInt k1 = (eval c1 env) and VInt k2 = (eval c2 env) in (k1 < k2)
  |G -> let VInt k1 = (eval c1 env) and VInt k2 = (eval c2 env) in (k1 > k2)
  |Eq -> let VInt k1 = (eval c1 env) and VInt k2 = (eval c2 env) in (k1 == k2) 
  |Ne -> let VInt k1 = (eval c1 env) and VInt k2 = (eval c2 env) in (k1 <> k2)
  (*|And -> let VBool k1 = (eval c1 env) and VBool k2 = (eval c2 env) in (k1 && k2)
  |Or -> let VBool k1 = (eval c1 env) and VBool k2 = (eval c2 env) in (k1 || k2);; *) (* problème avec And/ Or que je dois régler*)
