foo::[Int]->[Int]->[Int]
foo x y = fooHelper x y 1

fooHelper::[Int]->[Int]->Int->[Int]
fooHelper [] [] a = []
fooHelper x y a 
  | ((mod a 2 ) == 1) = take a x ++ fooHelper (drop a x) y (a+1)
  | ((mod a 2 ) == 0) = take a y ++ fooHelper x (drop a y) (a+1)


main = do
  print(foo [1,2,3,4,5,6,7,8] [11,12,13,14,15,16,17,18])