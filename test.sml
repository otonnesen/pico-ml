let rec range = fun s -> fun e ->
  if s > e - 1 then [] else s::range (s + 1) e in
let rec map = fun f -> fun l ->
  match l with [] -> [] | x::y -> (f x)::(map f y) in
let l1 = range 0 10 in
let twice = fun x -> x * 2 in
map twice l1
