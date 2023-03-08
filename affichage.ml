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
  | MNom s -> s
  | MCouple (m1,m2) -> "(" ^ (get_var m1) ^ ", " ^ (get_var m2) ^ ")"
  | MNone  -> "'a"
  | MUnit  -> "()"
  | MCons (m1,m2) ->  "(" ^ (get_var m1) ^ "::" ^ (get_var m2) ^ ")"
  | MEmptyList -> "[]"
  | MExpr e1 -> affiche_expr_tree e1; "" 

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
  | TryWith (e1,e2,e3) -> aff_aux3 "TryWith(" e1 e2 e3
  | InDecr (e1,b)            -> aff_aux1 (if b then "Incr(" else "Decr(") e1
  | Fsd (e1,b)            -> aff_aux1 (if b then "Fst(" else "Snd(") e1
  | Cons (e1,e2)       -> aff_aux2 "Cons(" e1 e2
  | EmptyList          -> print_string "[]"
  | MatchWith (e1,l)   -> aff_aux1 "MatchWith(" e1 ; List.iter (fun (motif,expr) -> affiche_var motif ; print_string ", "; affiche_expr_tree expr) l



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
  | Let (recursif,x,e1,b,e2)-> print_string ("let " ^ if recursif then "rec "else "") ; affiche_var x; print_string " = "; affiche_expr e1
 (* | LetRec (x,e1,e2)   -> print_string "let rec "; print_string x; print_string " = "; affiche_expr e1; print_string " in "; affiche_expr e2*)
  | Fun (arg,e1)       -> print_string "fun "; affiche_var arg; print_string" -> "; affiche_expr e1 
  | App (e1,e2)        -> affiche_expr e1; print_string " "; (match e2 with
            | Const _
            | BConst _
            | Var _ -> affiche_expr
            | _ -> print_parenthese) e2
  | Seq (e1,e2)        -> affiche_expr e1; print_string ";\n"; affiche_expr e2

  | _ -> print_string "later"


let rec affiche_val v = match v with
  | VInt k            -> print_string "int = "; print_int k
  | VBool b           -> print_string "bool = "; print_string (if b then "true" else "false")
  | VFun (arg,e1,_,b) -> print_string "<fun> = "; if b then print_string "(rec) "; affiche_var arg; print_string" -> "; affiche_expr e1 
  | VUnit             -> print_string "unit = ()"
  | VRef k            -> print_string "ref = {contents = "; affiche_val ref_memory.(k); print_string "}"
  | VTuple (v1,v2)    -> print_string "tuple = "; affiche_val v1; print_string ", "; affiche_val v2
  | VList _ -> ()
  | VExcep (n,b)      -> if b then print_string "[ERROR] "; print_string "exn = E "; print_int n 
                      



let rec display_env env = match env with
  | (cle,valeur) :: env' -> display_env env'; print_string cle; print_string " : "; affiche_val valeur; print_string "   /   "
  | [] -> ()

  
let print_env env = print_newline (); print_string "Environnement -> " ; display_env env; print_newline ()



let print_debug e = print_string ("Je suis dans " ^ (match e with
  | Const i   -> "Const " ^ (string_of_int i)
  | BConst _  -> "BConst"
  | Var s     -> "Var " ^ get_var s
  | Unit      -> "Unit"
  | ArithOp _ -> "ArithOp"
  | CompOp _  -> "CompOp"
  | BoolOp _  -> "BoolOp"
  | If _      -> "If"
  | PrInt _   -> "PrInt"
  | Fun _     -> "Fun"
  | App _     -> "App"
  | Let _     -> "Let"
  | Seq _     -> "Seq" 
  | Ref _     -> "Ref"
  | ValRef _  -> "ValRef"
  | RefNew _  -> "RefNew"
  | Exn _     -> "Exn"
  | Raise _   -> "Raise"
  | TryWith _ -> "TryWith"
  | InDecr _    -> "Incr"
  | _ -> "TO DO")
  ^ (if not !Options.slow then "\n" else ""))