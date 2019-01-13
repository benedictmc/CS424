weaveHunks::Int->[a]->[a]->[a]
weaveHunks t x y = weaveHunksHelp t x y 1

weaveHunksHelp::Int->[a]->[a]->Int->[a]
weaveHunksHelp _ [] [] _ = []
weaveHunksHelp t x y a 
  | ((mod a 2 ) == 1) = take t x ++ weaveHunksHelp t (drop t x) y (a+1)
  | ((mod a 2 ) == 0) = take t y ++ weaveHunksHelp t x (drop t y) (a+1)

main = do
  print(weaveHunks 2 [1..10] [11..20])