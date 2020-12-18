import Data.Array
import Data.List (nub, elemIndex)

type Row = Int
type Col = Int
type Offset = Int

parse :: String -> (Int, Array (Row,Col) Int)
parse s = (r, array ((1,1),(m,n)) indexed_hs)
    where
        ([m,n,r]:ls) = map (map (read :: String -> Int) . words) $ lines s
        indexed_hs = concat $ map f $ zip [1..] $ map (zip [1..]) ls :: [((Row, Col), Int)]
        f (i,jxs) = map (g i) jxs
        g i (j,x) = ((i,j), x)

rotate :: Int -> Array (Row,Col) a -> Array (Row,Col) a
rotate r arr = ixmap ((1,1),(m,n)) rotater arr
    where
        ((_,_),(m,n)) = bounds arr
        layer (i,j) = foldr1 min $ [m-i,i-1,n-j,j-1]
        len (i,j) = 
            let l = layer (i,j)
            in 2 * ((m - (2 * l) - 1) + (n - (2 * l) - 1))
        shift (i,j) = r `mod` (len (i,j))
        rotater (i,j) = clockwise (i,j) (shift (i,j))
        clockwise :: (Row,Col) -> Offset -> (Row,Col)
        clockwise (i,j) x = cycle !! ((index + x) `mod` (len (i,j)))
            where
                l = layer (i,j)
                cycle = nub $ top ++ right ++ bottom ++ left
                index = case elemIndex (i,j) cycle of
                    Nothing -> error "Index not found"
                    (Just i) -> i
                top = zip (repeat (l+1)) [(l+1)..(n-l)]
                right = zip [(l+1)..(m-l)] (repeat (n-l))
                bottom = zip (repeat (m-l)) (reverse [(l+1)..(n-1)])
                left = zip (reverse [(l+1)..(m-l)]) (repeat (l+1))

toLists :: Array (Row,Col) a -> [[a]]
toLists arr = splitAfter n $ map (arr !) $ range (bounds arr)
    where ((_,_),(m,n)) = bounds arr

splitAfter :: Int -> [a] -> [[a]]
splitAfter _ [] = []
splitAfter n xs = take n xs : splitAfter n (drop n xs)

display :: [[Int]] -> String
display = unlines . map (unwords . map show)

solution :: String -> String
solution = display . toLists . uncurry rotate . parse

main :: IO ()
main = interact solution

test :: IO ()
test = do
    s <- readFile "test.in"
    putStr $ solution s
