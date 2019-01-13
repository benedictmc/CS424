mapSkip::(Int->Int)->[Int]->[Int]
mapSkip x [] = []
mapSkip x [y] = [(x y)]
mapSkip x (y:y1:ys) = [(x y)] ++ [y1] ++ mapSkip x ys

main = do    
    print(mapSkip (+1000) [1..7])
