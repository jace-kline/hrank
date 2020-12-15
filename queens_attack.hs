
data Direction = DownL
               | UpR
               | UpL
               | DownR
               | L
               | R
               | Down
               | Up
               deriving (Eq, Show, Enum)

type Row = Int
type Col = Int
type Distance = Int

type Queen = (Row,Col)
type Hazard = (Row,Col)
type DirDists = [Distance] -- for each direction (enumed), notes the distance that can be reached. Updated by hazards

dirDist :: Queen -> Hazard -> Maybe (Direction, Distance)
dirDist (rq,cq) (r,c)   | (rq - r == cq - c && rq - r > 0) = return (DownL, rq - r - 1)
                        | (rq - r == cq - c && rq - r < 0) = return (UpR, r - rq - 1)
                        | (rq - r == c - cq && rq - r > 0) = return (UpL, rq - r - 1)
                        | (rq - r == c - cq && rq - r < 0) = return (DownR, r - rq - 1)
                        | (r == rq && cq - c > 0) = return (L, cq - c - 1)
                        | (r == rq && cq - c < 0) = return (R, c - cq - 1)
                        | (c == cq && rq - r > 0) = return (Down, rq - r - 1)
                        | (c == cq && rq - r < 0) = return (Up, r - rq - 1)
                        | otherwise = Nothing

initDirDists :: Int -> Queen -> DirDists
initDirDists n (rq,cq) = [min (rq - 1) (cq - 1),
    min (n - rq) (n - cq), 
    min (n - rq) (cq - 1),
    min (rq - 1) (n - cq),
    cq - 1,
    n - cq,
    rq - 1,
    n - rq]

updateDirDists :: Queen -> Hazard -> DirDists -> DirDists
updateDirDists q h m = case dirDist q h of
    Nothing -> m
    Just (dir,dist) -> 
        let i = fromEnum dir
        in if (m !! i) <= dist 
            then m
            else take i m ++ (dist : drop (i + 1) m)

solve :: Int -> Queen -> [Hazard] -> Int
solve n q hs = sum $ foldr f (initDirDists n q) hs
    where
        f :: Hazard -> DirDists -> DirDists
        f h m = updateDirDists q h m


parse :: String -> (Int, Queen, [Hazard])
parse s = 
    let ([n,_]:[rq,cq]:rest) = map (map (read :: String -> Int) . words) $ lines s
    in (n,(rq,cq),map mkPair rest)


mkPair :: [a] -> (a,a)
mkPair [x,y] = (x,y)
mkPair _ = error "mkPair expected list of length 2"

solution :: String -> String
solution s = 
    let (n,q,hs) = parse s
    in show $ solve n q hs

main :: IO ()
main = interact solution

test :: IO ()
test = do
    s <- readFile "test.in"
    putStrLn $ solution s