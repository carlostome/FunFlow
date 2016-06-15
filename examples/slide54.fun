let f = fn x => x + 1 in
let g = fn y => y * 2 in
let h = fn z => if z then f else g in
f
