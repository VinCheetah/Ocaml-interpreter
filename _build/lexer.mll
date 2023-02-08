{
  open Parser
exception Eof
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t']                                          { token lexbuf }    (* on saute les blancs et les tabulations *)
 	     	   	           (* token: appel récursif *)
                                   (* lexbuf: argument implicite
                                      associé au tampon où sont
                                      lus les caractères *)
  | '\n'                                     { EOL }
  | '+'                                      { PLUS }
  | '*'                                      { TIMES }
  | '-'                                      { MINUS }
  | "/"                                      { DIV }
  | "mod"                                    { MOD }
  | '('                                      { LPAREN }
  | ')'                                      { RPAREN }
  | "if"                                     { IF }
  | "then"                                   { THEN }
  | "else"                                   { ELSE }
  | "="                                      { EQ }
  | ">"                                      { G }
  | ">="                                     { GE } 
  | "<>"                                     { NE }
  | '<'                                      { L }
  | "<="                                     { LE }
  | "true"                                   { TRUE }
  | "false"                                  { FALSE }
  | "&&"                                     { AND }
  | "||"                                     { OR }
  | "not"                                    { NOT }
  | ['0'-'9']+ as s                          { INT (int_of_string s) }
  | "prInt"                                  { PRINT }
  | "let"                                    { LET }
  | "in"                                     { IN }
  | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9']* as s { VAR (s) }
  | eof                                      { raise Eof } (* fin du fichier *)
