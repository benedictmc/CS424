tear::(Int->Bool)->[Int]->[[Int]]
tear x y = [tearT x y, tearF x y]

tearT::(Int->Bool)->[Int]->[Int]
tearT x [] = []
tearT x (y:ys)
  | (x y) = [y] ++ tearT x ys
  | otherwise = tearT x ys

tearF::(Int->Bool)->[Int]->[Int]
tearF x [] = []
tearF x (y:ys)
  | (x y) = tearF x ys
  | otherwise = [y] ++ tearF x ys

main = do
  print(tear (>5) [1,10,2,12,3,13])
