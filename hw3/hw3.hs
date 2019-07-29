--function that takes in a list and outputs a list of lists
--the first member of the output list will be the same as input
--the second member will contain every second element from the input list
skips :: [a] -> [[a]]
skips xs = [allOfN xs i | i <- [1 .. length xs]]

--fucntion that takes a list of intergers and returns the local maximum
--here the local maximum is the number that is bigger than the element on it's left
--it's right.
localMaxima :: [Int] -> [Int]
localMaxima (x:y:z:zs)
    | x < y && y > z = y : localMaxima (y:z:zs)
    | otherwise      = localMaxima(y:z:zs)
localMaxima _ = []

--helper function that takes Ints amd returns a * if it matcheds otherwise ruterns space
findHisto :: Int -> Int -> [Int] -> String
findHisto x y l = if length (filter (x ==) l) > y then "*" else " "

--another helper function that takes a list int and returns an int by finding the maximum
highestHisto :: [Int] -> Int
highestHisto l = maximum [length (filter (x ==) l) | x <- [0..9] ]

--last helper fucntion that takes a list and an int and returns list
allOfN :: [a] -> Int -> [a]
allOfN xs n = case drop (n - 1) xs of
    y:ys -> y : allOfN ys n
    []   -> []

--last part of the homework that will take a list of integers and returns a string 
--for an input like histogram [3,5] it will return "* *\n==========\n0123456789\n
--to het a proper output for this function, we will need to use putStr before the actual example
-- for example putStr(histogram [3,5])
histogram :: [Int] -> String
histogram l = concat [concat [findHisto x y l | x <- [0 .. 9]]
                             ++ "\n" | y <- reverse [0 .. highestHisto l - 1]] 
                     ++ "==========" ++ "\n0123456789\n"


