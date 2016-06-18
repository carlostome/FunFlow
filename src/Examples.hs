module Examples where

import Main

p0 = "Pair(Pair(fn x => x, Cons(1,Nil)), true)"

-- Pair projections
proj0 =
  "let fst = fn p => pcase p of Pair(x,y) => x in \
  \let snd = fn p => pcase p of Pair(x,y) => y in \
  \ fst                                           "

-- proj applied
proj1 =
  "let fst = fn p => pcase p of Pair(x,y) => x in \
  \let snd = fn p => pcase p of Pair(x,y) => y in \
  \let p = Pair(fn x => x, Cons(1,Nil)) in        \
  \ fst p                                         "

-- proj applied
proj2 =
  "let fst = fn p => pcase p of Pair(x,y) => x in \
  \let snd = fn p => pcase p of Pair(x,y) => y in \
  \let p = Pair(fn x => x, Cons(1,Nil)) in        \
  \ snd p                                         "

-- create a pair
pair0 =
  "let pair = fn x =>  fn y => Pair(x,y) in  \
  \ pair                                     "

pair1 =
  "let pair = fn x =>  fn y => Pair(x,y) in                          \
  \let p = pair (Cons(fn x => x + 1, Cons(fn z => z * 2, Nil))) 1 in \
  \ pcase p of Pair(x,y) => x                                        "

-- create and destruct a pair of pairs
pair2 =
  "let pair = fn x =>  fn y => Pair(x,y) in       \
  \let fst = fn p => pcase p of Pair(x,y) => x in \
  \let snd = fn p => pcase p of Pair(x,y) => y in \
  \ fst (pair (fn z => z + 1) 2)                   "

l0 = "Cons (Pair(fn x => 1, 2), Cons(Pair(fn x => 2, 3), Nil))"

l1 = "let l = if true                                                                \
     \   then Cons(Pair(fn x => x,Pair(1,2)), Cons(Pair(fn z => z, Pair(3,4)),Nil))  \
     \   else Nil in                                                                 \
     \ (fn x => x) l                                                                 "


-- traverse a list and do nothing
trav0 =
  "fun traverse list =>                   \
  \ lcase list of                         \
  \    Cons(x,xs) => Cons(x, traverse xs) \
  \               or Nil                  "

-- traverse applied to a list of pairs
trav1 =
  "let trav = fun traverse list =>                  \
  \ lcase list of                                   \
  \    Cons(x,xs) => Cons(x, traverse xs)           \
  \               or Nil in                         \
  \  trav (Cons (Pair(1,2), Cons(Pair(3,4),Nil)))   "

-- map function
map1 =
  "fn f => fun map list =>             \
  \ lcase list of                      \
  \    Cons(x,xs) => Cons(f x, map xs) \
  \               or Nil               "

-- map applied to a list of int
map2 =
  "let map = fn f => fun map list =>     \
  \   lcase list of                      \
  \     Cons(x,xs) => Cons(f x, map xs)  \
  \                or Nil in             \
  \ let l = Cons(1,Cons(2,Nil)) in       \
  \   map (fn x => x + 1) l              "

-- apply functions on a list to 1
map3 =
  "let map = fn f => fun map list =>                               \
  \   lcase list of                                                \
  \     Cons(x,xs) => Cons(f x, map xs)                            \
  \                or Nil in                                       \
  \ let l = Cons ((fn x => x + 1), Cons ((fn z => z * 7), Nil)) in \
  \ map (fn f => f 1) l                                            "

-- tail
tail0 =
  "fn list =>              \
  \  lcase list of         \
  \    Cons(x,xs) => xs    \
  \               or list  "

-- tail applied
tail1 =
  "let tail = fn list =>             \
  \ lcase list of                    \
  \    Cons(x,xs) => xs              \
  \               or list in         \
  \let l = Cons (Pair(1,2),Nil)  in  \
  \ tail l                           "

-- head function with default element
head0 =
  "let head = fn def => fn list =>   \
  \ lcase list of                    \
  \    Cons(x,xs) => x               \
  \               or def in          \
  \ head                             "

-- head applied
head1 =
  "let head = fn def => fn list =>         \
  \ lcase list of                          \
  \    Cons(x,xs) => x                     \
  \               or def in                \
  \ let l = (Cons (fn x => 1,Nil)) in      \
  \  head (fn x => x) l                    "

