open Expr
open Affichage


(* "incantations" qu'il n'est pas n�cessaire de comprendre dans un premier
   temps : on r�cup�re l'entr�e, dans un fichier ou sur le clavier *)
let nom_fichier = ref ""

let recupere_entree () =
  Arg.parse [] (fun s -> nom_fichier := s) "";
  try
    let where_from = match !nom_fichier with
      | "" -> stdin
      | s -> open_in s in
    let lexbuf = Lexing.from_channel where_from in
    let parse () = Parser.main Lexer.token lexbuf in
    parse () 
  with e -> (Printf.printf "probl�me de saisie\n"; raise e)

(* mettre � true et recompiler si l'on veut voir l'ex�cution pas � pas de l'automate *)
let trace = ref false
let _ = Stdlib.Parsing.set_trace !trace


      
      
(* le traitement d'une expression en entr�e *)   
let execute e =
  begin
    affiche_expr e; print_newline();
    (*affiche_expr e;*) (* Il faut enlever les commentaires sur les deux lignes si on veut voir le code caml généré par les fonctions d'affichages , dans cet état, *)
    (*print_newline();*) (* on ne voit que l'expression demandée si on inscrit le mot prInt et on n'a pas le code caml demandée à la question 1 *)
    let v =  Expr.eval e Expr.empty_env in
    affiche_val v;
    print_newline()
  end

(* la boucle principale *)
let calc () =
  try
      let saisie = recupere_entree () in
	execute saisie; flush stdout
  with e -> raise e


let _ = calc()
