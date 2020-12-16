import Data.List (transpose)

main :: IO ()
main = interact solve

test :: IO ()
test = do
    s <- readFile "test.in"
    putStrLn $ solve s

solve :: String -> String
solve s = unwords $ transpose $ takeRows n_cols xs
    where
        xs = concat $ words s
        l = length xs
        n_cols = ceiling $ sqrt $ fromIntegral l :: Int

takeRows :: Int -> String -> [String]
takeRows n [] = []
takeRows n s = if (length s < n)
    then [s]
    else take n s : takeRows n (drop n s)

-- transpose :: [[a]] -> [[a]]
-- transpose [] = []
-- transpose xxs = map (fromMaybe . sequence . filter (\x -> case x of { Nothing -> False; _ -> True })) mxxsT
--     where
--         maxlen = foldr (\xs l -> let l' = length xs in 
--             if l' > l then l' else l) 0 xxs
--         mxxs = map f xxs
--         f xs = map Just xs ++ take (maxlen - length xs) (repeat Nothing)
--         mxxsT = [map (!! i) mxxs | i <- [0..(maxlen - 1)]]
--         fromMaybe (Just x) = x