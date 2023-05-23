let rec f l p = match l with
|[] -> []
|a::q -> (a,p)::(f q p) in f