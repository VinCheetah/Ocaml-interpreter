open Types

exception TypeError of (t*t);;
exception Not_unifyable;;
exception Impossible_occ of (t*t);;
exception EvalError of string;;