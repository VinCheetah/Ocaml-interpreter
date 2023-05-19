open Expr
open Types
open Affichage
open Options
open Unif
open Inference
open Exceptions

(* "incantations" qu'il n'est pas n�cessaire de comprendre dans un premier
   temps : on r�cup�re l'entr�e, dans un fichier ou sur le clavier *)
let nom_fichier = ref ""


let recupere_entree () =
  let speclist = [("-showsrc",Arg.Set Options.showsrc,"Print an ocaml code of the input");("-debug",Arg.Set Options.debug,"Debug activated");
                  ("-slow",Arg.Set Options.slow,"Eval function works step by step");("-tree",Arg.Set Options.tree,"Tree activated");
                  ("-trace",Arg.Set Options.trace,"Trace activated");("-warnings",Arg.Set Options.warnings,"Warnings activated");
                  ("-output",Arg.Set Options.output,"Show Output");  ("-showtypes",Arg.Set Options.showtypes, "Show Types");
                  ("-notypes",Arg.Set Options.notypes,"No Types"); ("-showinf", Arg.Set Options.showinf, "Show inference progression")] in 
  Arg.parse speclist (fun s -> nom_fichier := s) "Bienvenue sur Fouine 1.0"; actu_options (); 
  let _ = Stdlib.Parsing.set_trace !Options.trace in
    let where_from = match !nom_fichier with
      | "" -> stdin
      | s -> open_in s in
    let lexbuf = Lexing.from_channel where_from in
    let parse () = Parser.main Lexer.token lexbuf in
    parse ()

(* mettre � true et recompiler si l'on veut voir l'ex�cution pas � pas de l'automate 
let trace = ref true
let _ = Stdlib.Parsing.set_trace !trace
*)

      
      
(* le traitement d'une expression en entr�e *)   
let execute e =
  begin 
    let e = identifie_variables e in
    let _ = inf e in 
    if !Options.showinf then (print_prob !inference);
    if not !Options.notypes then (add_inf (Var("-", find_type e, true), None); print_typage (unify !inference); if !Options.showinf then affiche_compt !compt compt_max);
    if !Options.tree then (affiche_expr_tree e; print_newline ());
    if !Options.showsrc then (affiche_expr_final e; print_string ";;\n");
    let v = Expr.eval e Types.empty_env in if !Options.output then (print_string "\nout : "; affiche_val v; print_newline ()) 
  end

(* la boucle principale *)
let calc () =
  try
    let saisie = recupere_entree () in
    execute saisie; flush stdout
  with e -> raise e


let _ = try calc() with
| Stdlib.Parsing.Parse_error -> print_string "Error : problème de saisie\n"
| TypeError (t1, t2) -> print_string ("Type Error : "^print_type t1^" - "^print_type t2^"\n")
| EvalError commentaire -> print_string ("Error : l'évaluation a échouée avec le commentaire suivant : \n"^commentaire^"\n")
| Not_unifyable -> print_string ("Problème d'unification\n")
