
import Data.List (unlines, sort)

type Index = Int

solve :: String -> Maybe String
solve w = case reorder w of
    (False,_) -> Nothing
    (True, w') -> return w'

reorder :: String -> (Bool, String)
reorder [] = (False, [])
reorder (x:xs) = case reorder xs of
    (False,_) -> case trySwapMinGreater (x:xs) of
        Nothing -> (False, (x:xs))
        Just (y:ys) -> (True, y : sort ys)
    (True,ys) -> (True,x:ys)

trySwapMinGreater :: Ord a => [a] -> Maybe [a]
trySwapMinGreater [] = Nothing
trySwapMinGreater l@(x:xs) = case findIndex (zip [1..] xs) of
    0 -> Nothing
    i -> return $ ((l !! i):(tail (take i l))) ++ (x : drop (i + 1) l)
    where
        findIndex ixs = go Nothing ixs
        go Nothing [] = 0
        go (Just (i,v)) [] = i
        go Nothing ((j,t):ixs) = go (if t > x then return (j,t) else Nothing) ixs
        go (Just (i,v)) ((j,t):ixs) = go (return (if t > x && t < v then (j,t) else (i,v))) ixs


parse :: String -> [String]
parse s = tail $ lines s

solution :: String -> String
solution s = unlines $ map (showSol . solve) $ parse s

showSol :: Maybe String -> String
showSol Nothing = "no answer"
showSol (Just w) = w

main :: IO ()
main = interact solution

test :: IO ()
test = do
    s <- readFile "test.in"
    putStr $ solution s
