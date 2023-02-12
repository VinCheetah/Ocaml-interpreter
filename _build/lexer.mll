{
  open Parser
exception Eof
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t' '\n']                          { token lexbuf }    (* on saute les blancs et les tabulations *)
 	     	   	           (* token: appel récursif *)
                                   (* lexbuf: argument implicite
                                      associé au tampon où sont
                                      lus les caractères *)
  | ";;"                                     { EOF } (* fin du fichier *)
  | ";"                                      { SCOLON }
  | '+'                                      { PLUS }
  | '*'                                      { TIMES }
  | '-'                                      { MINUS }
  | "/"                                      { DIV }
  | "mod"                                    { MOD }
  | "()"                                     { UNIT }
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
  | "fun"                                    { FUN }
  | "rec"                                    { REC }
  | "->"                                     { FLECHE }
  | "_"                                      { UNDERSCORE }
  | "ref"                                    { REF }
  | ":="                                     { REVAL }
  | "!"                                      { EXCL }
  | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9']* as s { VAR (s) }
 

