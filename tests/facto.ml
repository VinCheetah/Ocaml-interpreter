let rec facto = fun n -> if n = 0 then 1 else n * facto (n-1) in facto 5;;