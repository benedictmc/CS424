-- Author: Benedict McGovern
-- 15340696

-- Plus Code:
plus :: Int -> [Int] -> [Int] -> [Int]
plus b x y = plusHelp b 0 x y

plusHelp :: Int -> Int -> [Int] -> [Int] -> [Int]
plusHelp b c x y
    | x == [] && y == [] && c == 0 = []
    | x == [] && y == [] && c /=0 = [c]
    | x == [] = plusHelp b c [0] y
    | y == [] = plusHelp b c x [0]
plusHelp b c (x:xs) (y:ys) = [mod (x+y+c) b] ++ (plusHelp b (get_c b x y c) xs ys)

get_c :: Int -> Int -> Int -> Int -> Int
get_c b x y c 
    | c == 0 = quot (x+y) b
    | x+y+c == 10 && x /=0 && y /=0 = c+1
    | otherwise = (quot (x+y+c) b)

-- Times Code:
times :: Int -> [Int] -> [Int] -> [Int]
times b x y = timesOne b 0 0 x y

timesOne :: Int -> Int -> Int -> [Int] -> [Int] -> [Int]
timesOne b c1 c2 x y
    | x == [] && y == [] = []
    | x == [] = [c1 + c2] ++ (timesOne b c1 c2 [] (tail y))
    | y == [] = [c1 + c2] ++ (timesOne b c1 c2 (tail x) [])
timesOne b c1 c2 (x:xs) (y:ys) = [(mod ((x*y)) b) ]  ++ (timesOne b (get_c1_mul b x y) (get_c2_mul b x y) xs ys)


get_c1_mul :: Int -> Int -> Int -> Int
get_c1_mul b x y = (quot (x*y) b)

get_c2_mul :: Int -> Int -> Int -> Int
get_c2_mul b x y = (mod ((x*y)) b)



main = do
    -- Plus Examples:
    print(plus 10[7,2,1] [9,1,1])
    -- Using finite list of 9's to allow code to halt
    print(plus 10[1] [9,9,9,9,9,9,9,9,9,9,9])

    -- Times Examples:
    -- Using finite list of 9's to show idea
    print(times 10 [2] [9,9,9,9,9,9,9,9,9,9,9])
    print(times 10 [9] [9,9,9,9,9,9,9,9,9,9,9])