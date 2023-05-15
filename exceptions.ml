open Types

exception TypeError of (t*t);;
exception Not_unifyable;;

exception EvalError of string;;