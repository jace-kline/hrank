import Control.Monad.Writer.Lazy
import Data.Monoid

insertionShifts :: Ord a => [a] -> Writer (Sum Int) ()
insertionShifts xs = insertionShiftsZipper [] xs

insertionShiftsZipper :: Ord a => [a] -> [a] -> Writer (Sum Int) ()
insertionShiftsZipper _ [] = tell $ Sum 0
insertionShiftsZipper ys (x:xs) = do
    ys' <- insertShifts x ys
    insertionShiftsZipper ys' xs

insertShifts :: Ord a => a -> [a] -> Writer (Sum Int) [a]
insertShifts x [] = tell (Sum 0) >> return [x]
insertShifts x (y:ys) = if x >= y 
                         then tell (Sum 0) >> return (x:y:ys)
                         else tell (Sum 1) >> (y:) <$> insertShifts x ys

parse :: String -> [[Int]]
parse = map snd . filter arrLine . (\ls -> zip [0..] ls) . map (map (read :: String -> Int) . words) . lines
    where
        arrLine (i,xs) = (i /= 0) && (i `mod` 2 == 0)

solve :: [[Int]] -> [Int]
solve = map (getSum . execWriter . insertionShifts)

solution :: String -> String
solution = unlines . map show . solve . parse

main :: IO ()
main = interact solution

test :: IO ()
test = do
    s <- readFile "test.in"
    putStr $ solution s