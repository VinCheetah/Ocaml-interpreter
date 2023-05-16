open Types
open Options

(* fonction d'affichage comparateur *) 
let affiche_comp = function
  | L  -> " < "
  | Le -> " <= "
  | G  -> " > "
  | Ge -> " >= "
  | Eq -> " = "
  | Ne -> " <> "

(* fonction d'affichage opérateurs arithmétiques *)
let affiche_arith = function
  | Add -> " + "
  | Min -> " - "
  | Mul -> " * "
  | Div -> " / "
  | Mod -> " mod "

(* fonction d'affichage opérateurs booléens *)
let affiche_bool = function
  | And -> " && "
  | Or  -> " || "
  | Not -> "not "


let rec get_var = function (* Fonction d'affichage des motifs*)
  | MNom s          -> if s = "_" then "_" else s (* nom de variable peu utilisé :) *)
  | MCouple (m1,m2) -> "(" ^ (get_var m1) ^ ", " ^ (get_var m2) ^ ")"
  | MNone           -> "_"
  | MUnit           -> "()"
  | MCons (m1,m2)   ->  "(" ^ (get_var m1) ^ "::" ^ (get_var m2) ^ ")"
  | MEmptyList      -> "[]"
  | MExpr e1        -> affiche_expr_tree e1; "" 
  | MExcp m1        -> "E "^(get_var m1)

and affiche_var v = print_string (get_var v)

and affiche_expr_tree e = (* Fonction permettant d'afficher les expressions sous forme d'arbres*)
  let aff_aux1 s a =
    begin
    print_string s;
    affiche_expr_tree a;
    print_string ")"
    end
  and aff_aux2 s a b =  
      begin
  print_string s;
  affiche_expr_tree a;
  print_string ", ";
  affiche_expr_tree b;
  print_string ")"
      end
  and aff_aux3 s a b c =
    begin
  print_string s;
  affiche_expr_tree a;
  print_string ", ";
  affiche_expr_tree b;
  print_string ", ";
  affiche_expr_tree c;
  print_string ")";
    end
  in
  match e with
  | Const k            -> print_int k
  | BConst b           -> print_string (if b then "true" else "false") 
  | Var s              -> affiche_var s
  | Unit               -> print_string "()"
  | CoupleExpr (e1,e2) -> aff_aux2 "CoupleExpr(" e1 e2
  | ArithOp (op,e1,e2) -> begin match op with
      | Add            -> aff_aux2 "Add(" e1 e2
      | Mul            -> aff_aux2 "Mul(" e1 e2
      | Min            -> aff_aux2 "Min(" e1 e2
      | Div            -> aff_aux2 "Div(" e1 e2
      | Mod            -> aff_aux2 "Mod(" e1 e2
                          end
  | BoolOp (op,e1,e2)  -> begin match op with
      | Or             -> aff_aux2 "Or(" e1 e2
      | And            -> aff_aux2 "And(" e1 e2
      | Not            -> aff_aux1 "Not(" e1
                          end
  | CompOp (op,e1,e2)  -> begin match op with
      | G              -> aff_aux2 "G(" e1 e2
      | Ge             -> aff_aux2 "Ge(" e1 e2
      | L              -> aff_aux2 "L(" e1 e2
      | Le             -> aff_aux2 "Le(" e1 e2
      | Eq             -> aff_aux2 "Eq(" e1 e2
      | Ne             -> aff_aux2 "Ne(" e1 e2
                          end
  | If (e1,e2,e3)      -> aff_aux3 "If(" e1 e2 e3
  | PrInt e1           -> aff_aux1 "prInt(" e1
  | Let (b1,s,e1,b2,e2)-> aff_aux3 ("Let"^(if b1 then "Rec(" else "(")) (Var s) e1 e2
  | Fun (s,e1)         -> aff_aux2 "Fun(" (Var s) e1
  | App (e1,e2)        -> aff_aux2 "App(" e1 e2
  | Seq (e1,e2)        -> aff_aux2 "Seq(" e1 e2
  | Ref e1             -> aff_aux1 "Ref(" e1
  | ValRef e1          -> aff_aux1 "ValRef(" e1
  | RefNew (e1,e2)     -> aff_aux2 "RefNew(" e1 e2
  | Exn (e1)           -> aff_aux1 "Exn(" e1
  | Raise e1           -> aff_aux1 "Raise(" e1
  | TryWith (e1,l)     -> print_string "TryWith(" ; affiche_expr_tree e1 ; List.iter (fun (motif,expr) -> print_string (", "^(get_var motif)^" -> "); affiche_expr_tree expr) l; print_string ")"
  | InDecr (e1,b)      -> aff_aux1 (if b then "Incr(" else "Decr(") e1
  | Cons (e1,e2)       -> aff_aux2 "Cons(" e1 e2
  | EmptyList          -> print_string "[]"
  | MatchWith (e1,l)   -> print_string "MatchWith("; affiche_expr_tree e1 ; List.iter (fun (motif,expr) -> print_string (", "^(get_var motif)^" -> "); affiche_expr_tree expr) l; print_string ")"



let rec affiche_expr e = (* Fonction d'affichage des expressions *)
  let print_parenthese e = print_string "("; affiche_expr e ; print_string ")" in
  match e with
  | Const n            -> print_int n
  | BConst b           -> print_string (if b then "true" else "false")
  | Var x              -> affiche_var x
  | Unit               -> print_string "()"
  | ArithOp (op,e1,e2) -> (match e1 with
            | If _ -> print_parenthese
            | ArithOp (op',_,_) when op' = Add || op' = Min -> (match op with
                      | Div
                      | Mul -> print_parenthese
                      | _   -> affiche_expr)
            | _ -> affiche_expr) e1;
          print_string (affiche_arith op); (match e2 with
            | ArithOp (op',_,_) when op' = Add || op' = Min -> (match op with
                      | Div
                      | Mul -> print_parenthese
                      | _   -> affiche_expr)
            | _ -> affiche_expr) e2

  | CompOp (op,e1,e2)  -> (match e1 with
            | BoolOp _ 
            | If _ -> print_parenthese
            | _    -> affiche_expr) e1;
            print_string (affiche_comp op);
            (match e2 with
            | BoolOp _ -> print_parenthese
            | _        -> affiche_expr) e2

  | BoolOp (op,e1,e2)  -> begin match op with
            | Not -> print_string " not "; (match e1 with
                      | BConst _ -> affiche_expr
                      | _ -> print_parenthese) e1
            | And -> (match e1 with
                      | BoolOp (op',_,_) when op' = Or || op' = And -> print_parenthese
                      | If _ -> print_parenthese
                      | _ -> affiche_expr) e1;
                    print_string " && ";
                    (match e2 with
                      | BoolOp (op',_,_) when op' = Or -> print_parenthese
                      | _ -> affiche_expr) e2
            | Or -> (match e1 with
                      | If _ -> print_parenthese
                      | _ -> affiche_expr) e1;
                    print_string " || ";
                    affiche_expr e2
            end
  | If (c1,e1,e2)      -> print_string "if " ; affiche_expr c1; print_string " then " ;affiche_expr e1; print_string " else "; affiche_expr e2
  | PrInt (e1)         -> print_string "prInt "; (match e1 with
            | Const _  -> affiche_expr
            | _ -> print_parenthese) e1  
  | Let (recursif,x,e1,b,e2)-> print_string ("let " ^ if recursif then "rec "else "") ; affiche_var x; print_string " = "; affiche_expr e1; print_string (if not b then " in " else ";;\n"); print_parenthese e2
  | Fun (arg,e1)       -> print_string "fun "; affiche_var arg; print_string" -> "; affiche_expr e1 
  | App (e1,e2)        -> affiche_expr e1; print_string " "; (match e2 with
            | Const _
            | BConst _
            | Var _ -> affiche_expr
            | _ -> print_parenthese) e2
  | Seq (e1,e2)        -> print_parenthese ( e1); print_string ";\n";print_parenthese ( e2)
  | Ref e1             -> print_string "ref "; print_parenthese ( e1) 
  | ValRef e1          -> print_string "!"; print_parenthese ( e1)
  | RefNew (e1,e2)     -> affiche_expr e1; print_string " := "; print_parenthese ( e2)
  | Exn e1             -> print_string "E "; print_parenthese ( e1)
  | Raise e1           -> print_string "raise "; print_parenthese ( e1) 
  | TryWith (e1,l)     -> print_string "try " ; affiche_expr e1 ; print_string " with\n"; List.iter (fun (motif,expr) -> print_string ("| "^(get_var motif)^" -> "); affiche_expr expr; print_newline ()) l
  | InDecr (e1,b)      -> print_string (if b then "incr " else "decr "); print_parenthese ( e1)  
  | EmptyList          -> print_string "[]"
  | Cons (e1,e2)       -> print_parenthese ( e1); print_string " :: "; print_parenthese ( e2)
  | MatchWith (e1,l)   -> print_string "match " ; affiche_expr e1 ; print_string " with\n"; List.iter (fun (motif,expr) -> print_string ("| "^(get_var motif)^" -> "); affiche_expr expr; print_newline ()) l
  | CoupleExpr (e1,e2) -> print_string "("; print_parenthese e1; print_string ", "; print_parenthese e2; print_string ")"

let affiche_expr_final e = print_string "exception E of int;;\nlet prInt x = print_int x; print_newline ();x;;\n"; affiche_expr e

let rec affiche_val v = match v with
  | VInt k            -> print_string "int = "; print_int k
  | VBool b           -> print_string "bool = "; print_string (if b then "true" else "false")
  | VFun (arg,e1,e,b) -> print_string "<fun> = "; if b then print_string "(rec) "; affiche_var arg; print_string " / Mon env : "; display_env e; print_string" / Corps : "; affiche_expr e1
  | VUnit             -> print_string "unit = ()"
  | VRef k            -> print_string "ref = {contents = "; affiche_val ref_memory.(k); print_string "}"
  | VTuple (v1,v2)    -> print_string "tuple = "; affiche_val v1; print_string ", "; affiche_val v2
  | VList l           -> begin let rec aux = function
                            | a :: b :: l' -> affiche_val a; print_string "; "; aux (b::l')
                            | a :: l' -> affiche_val a; aux l'
                            | [] -> print_string "]"
                          in  print_string "["; aux l
                          end
  | VExcep (n,b)      -> if b then print_string "[ERROR] "; print_string "exn = E "; print_int n 
                      



and display_env env = match env with
  | (cle,valeur) :: env' -> display_env env'; print_string cle; print_string " : "; affiche_val valeur; print_string "   /   "
  | [] -> ()

  
let print_env env = print_newline (); print_string "Environnement -> " ; display_env env; print_newline ()



let print_debug e = print_string ("Je suis dans " ^ (match e with
  | Const i      -> "Const " ^ (string_of_int i)
  | BConst _     -> "BConst"
  | Var s        -> "Var " ^ get_var s
  | Unit         -> "Unit"
  | ArithOp _    -> "ArithOp"
  | CompOp _     -> "CompOp"
  | BoolOp _     -> "BoolOp"
  | If _         -> "If"
  | PrInt _      -> "PrInt"
  | Fun _        -> "Fun"
  | App _        -> "App"
  | Let _        -> "Let"
  | Seq _        -> "Seq" 
  | Ref _        -> "Ref"
  | ValRef _     -> "ValRef"
  | RefNew _     -> "RefNew"
  | Exn _        -> "Exn"
  | Raise _      -> "Raise"
  | TryWith _    -> "TryWith"
  | InDecr _     -> "Incr"
  | EmptyList    -> "EmptyList"
  | Cons _       -> "Cons"
  | MatchWith _  -> "MatchWith"
  | CoupleExpr _ -> "CoupleExpr"
  ^ (if not !Options.slow then "\n" else "")))



let rec print_type = function
  | Var (a,id,t,b) -> a ^ (if !Options.showtypes then " ("^(string_of_int id)^")" else "")^ " : " ^ print_type t
  | None           -> "'a"
  | T c            -> begin match c with
    | TInt             -> "int"
    | TBool            -> "bool"
    | TFun (arg,corps) -> (print_type arg) ^ " -> " ^ (print_type corps)
    | TUnit            -> "unit"
    | TRef r           -> print_type r ^ " ref"
    | TTuple (a,b)     -> (print_type a) ^ " * " ^ (print_type b) 
    | TList l          -> print_type l ^ " list"
    | TExn             -> "exn"
    end
  | Prime a        -> "'"^Char.escaped (Char.chr (a+96)) 



let rec print_prob = function
| []         -> print_newline ()
| (a,b):: l' -> print_string (print_type a); print_string " ----- "; print_string (print_type b); print_string "\n"; print_prob l' 
;;


let rec print_typage = function
  | Var (a,id,t,b) :: l' -> if b || !Options.showtypes then print_string (a ^ (if !Options.debug then string_of_int id else "")^ " : " ^ print_type t ^ "\n");
                            print_typage l'
  | _ :: l' -> print_typage l'
  | [] -> ()


let affiche_compt c cmax = print_string ("Compteur : "^string_of_int c^" (max : "^string_of_int cmax^")\n")