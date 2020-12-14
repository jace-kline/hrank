type Matrix a = [[a]]

magic :: Matrix Int
magic = [[8,1,6], [3,5,7], [4,9,2]]

reflectRows :: Matrix a -> Matrix a
reflectRows = reverse

reflectCols :: Matrix a -> Matrix a
reflectCols = transpose . reflectRows . transpose

transpose :: Matrix a -> Matrix a
transpose m = [map (!! i) m | i <- [0..(n - 1)]]
    where 
        n = minElement $ map length m

minElement :: (Ord a) => [a] -> a
minElement xs = foldr1 f xs
    where f x y = if x < y then x else y

squares :: [Matrix Int]
squares = (fs ++ map (\f -> f . transpose) fs) <*> [magic]
    where 
        fs = [id, 
            reflectCols, 
            reflectRows, 
            reflectCols . reflectRows]

parse :: String -> Matrix Int
parse s = map (map (read :: String -> Int) . words) $ lines s

minCost :: Matrix Int -> Int
minCost m = minElement $ map (cost m) squares

cost :: Matrix Int -> Matrix Int -> Int
cost m1 m2 = sum [rowCost r1 r2 | (r1, r2) <- zip m1 m2]
    where 
        rowCost r1 r2 = sum [absDiff x y | (x, y) <- zip r1 r2]

absDiff :: Int -> Int -> Int
absDiff x y = if x > y then x - y else y - x

compute :: String -> String
compute s = show $ minCost $ parse s

test :: IO ()
test = do
    s <- readFile "test.in"
    putStrLn $ compute s

main :: IO ()
main = interact compute