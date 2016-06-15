let g = fn x => Pair (1,2) in
let h = fun f y => Pair (3,4) in
let z = if true then g else h in
g
