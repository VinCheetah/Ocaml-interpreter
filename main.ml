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
                  ("-notypes",Arg.Set Options.notypes,"No Types"); ("-showinf", Arg.Set Options.showinf, "Show inference progression");
                  ("-resultinf",Arg.Set Options.resultinf,"Show inferences results")] in 
  Arg.parse speclist (fun s -> nom_fichier := s) "Bienvenue sur Fouine 1.0";
  let _ = Stdlib.Parsing.set_trace !Options.trace in
  try
    let where_from = match !nom_fichier with
      | "" -> stdin
      | s -> open_in s in
    let lexbuf = Lexing.from_channel where_from in
    let parse () = Parser.main Lexer.token lexbuf in
    parse ()
  with e -> (Printf.printf "problème de saisie\n"; raise e)

(* mettre � true et recompiler si l'on veut voir l'ex�cution pas � pas de l'automate 
let trace = ref true
let _ = Stdlib.Parsing.set_trace !trace
*)

      
      
(* le traitement d'une expression en entr�e *)   
let execute e =
  begin try 
    let _ = inf e in 
    if !Options.debug then (print_prob !inference);
    if not !Options.notypes then ((try add_inf (Var("-",0, find_type e, true), None); print_typage (unify !inference) with Not_unifyable -> failwith "ERROR UNIF"));
    if !Options.tree || !Options.debug then (affiche_expr_tree e; print_newline ());
    if !Options.showsrc || !Options.debug then (affiche_expr_final e; print_string ";;\n");
    let v = Expr.eval e Types.empty_env in if !Options.output then (print_string "\nout : "; affiche_val v; print_newline ()) 
  with 
    | TypeError (t1, t2) -> print_string "Type error"
    | EvalError commentaire -> print_string ("L'évaluation a échouée avec le commentaire suivant : \n"^commentaire^"\n")
  end

(* la boucle principale *)
let calc () =
  try
    let saisie = recupere_entree () in
	  execute saisie; flush stdout
  with e -> raise e


let _ = calc()
