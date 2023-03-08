let r = ref 3 in
let f g x = prInt 100;
  let res = g x in
  r:= x;
  res
in
let k = f (fun x -> x*x) 12 in
prInt (!r + k)
