let rec f p l = match l with 
|[] -> [p]
|a::q -> a::[p]::(f p q) in f

