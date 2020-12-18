import Data.Array

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
        clockwise (i,j) x | (x == 0 || point (i,j)) = (i,j)
                          | toppoint (i,j) = (i + x - 1, j)
                          | rightpoint (i,j) = (i, j - x + 1)
                          | top (i,j) = 
                              let j' = j + x
                              in if j' <= n - l 
                                  then (i,j')
                                  else clockwise (i,n-l) (x-(n-l-j))
                          | right (i,j) =
                              let i' = i + x
                              in if i' <= m - l
                                  then (i',j)
                                  else clockwise (m-l,j) (x-(m-l-i))
                          | bottom (i,j) =
                              let j' = j - x
                              in if j' >= l + 1 
                                  then (i,j')
                                  else clockwise (i,l+1) (x-(j-l-1))
                          | left (i,j) =
                              let i' = i - x
                              in if i' >= l + 1
                                  then (i',j)
                                  else clockwise (l+1,j) (x-(i-l-1))
                          | otherwise = error "Invalid point condition"
                          where l = layer (i,j)
        point (i,j) = top' (i,j) && left' (i,j) && bottom' (i,j) && right' (i,j)
        toppoint (i,j) = top' (i,j) && left' (i,j) && not (bottom' (i,j)) && right' (i,j)
        rightpoint (i,j) = top' (i,j) && not (left' (i,j)) && bottom' (i,j) && right' (i,j)
        top (i,j) = top' (i,j) && not (right' (i,j))
        right (i,j) = right' (i,j) && not (bottom' (i,j))
        bottom (i,j) = bottom' (i,j) && not (left' (i,j))
        left (i,j) = left' (i,j) && not (top' (i,j))
        top' (i,j) = i == layer (i,j) + 1
        right' (i,j) = j == n - layer (i,j)
        bottom' (i,j) = i == m - layer (i,j)
        left' (i,j) = j == layer (i,j) + 1

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
