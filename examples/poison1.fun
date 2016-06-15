let f = fn x => x in
let g = fun f x => x in
let h = if true then f else g in
f
