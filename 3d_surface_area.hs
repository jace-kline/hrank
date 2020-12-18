import Data.Array

type Height = Int
type SurfaceArea = Int
type Row = Int
type Col = Int

type ToyArray = Array (Row, Col) Height

parse :: String -> ToyArray
parse s = array ((1,1),(r,c)) indexed_hs
    where
        ([r,c]:ls) = map (map (read :: String -> Height) . words) $ lines s
        indexed_hs = concat $ map f $ zip [1..] $ map (zip [1..]) ls :: [((Row, Col), Height)]
        f (i,jxs) = map (g i) jxs
        g i (j,x) = ((i,j), x)

sa :: ToyArray -> SurfaceArea
sa arr = (2 * l * w) + sa_sides
    where
        ((r0,c0),(r,c)) = bounds arr
        l = r - r0 + 1
        w = c - c0 + 1
        sa_sides = sum $ map sa_index [(i,j) | i <- [r0..r], j <- [c0..c]]
        sa_index (i,j) = sum $ map (sa_side (i,j)) [(i-1,j),(i+1,j),(i,j-1),(i,j+1)]
        sa_side (i, j) (i',j') = 
            let h = arr ! (i,j) 
            in if inBounds (i',j')
                then 
                    let h' = arr ! (i',j')
                    in if h' > h then 0 else h - h'
                else h
        inBounds (i,j) = i >= r0 && i <= r && j >= c0 && j <= c

solution :: String -> String
solution s = show $ sa $ parse s

main :: IO ()
main = interact solution

test :: IO ()
test = do
    s <- readFile "test.in"
    putStrLn $ solution s