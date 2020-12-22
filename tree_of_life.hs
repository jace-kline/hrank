{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Control.Monad.Fail
import Control.Applicative
import Data.Char

newtype Parser t a = Parser { runParser :: [t] -> Maybe (a, [t]) }

-- Runs the parser and only returns parsed values if
-- the entire token stream has been consumed
evalParser :: Parser t a -> [t] -> Maybe a
evalParser p ts = do
    (v,ts) <- runParser p ts
    guard (null ts)
    return v

instance Functor (Parser t) where
    fmap f (Parser g) = Parser $ \ts -> fmap (\(x,y) -> (f x, y)) $ g ts

instance Applicative (Parser t) where
    pure x = Parser $ \ts -> pure (x, ts)
    -- liftA2 f (Parser g) (Parser h) = Parser $ \ts -> do
    --     (x, ts')  <- g ts
    --     (y, ts'') <- h ts'
    --     return (f x y, ts'')
    (Parser g) <*> (Parser h) = Parser $ \ts -> do
        (f, ts') <- g ts
        (y, ts'') <- h ts'
        return (f y, ts'')

instance Alternative (Parser t) where
    empty = Parser $ \_ -> Nothing
    (Parser g) <|> (Parser h) = Parser $ \ts -> 
        case g ts of
            Nothing  -> h ts
            x        -> x

instance Monad (Parser t) where
    return = pure
    (Parser g) >>= h = Parser $ \ts -> 
        case g ts of
            Just (x, ts') -> (runParser $ h x) ts'
            Nothing       -> Nothing
    -- fail _ = empty

instance MonadPlus (Parser t) where
    mzero = empty
    mplus = (<|>)

instance MonadFail (Parser t) where
    fail _ = empty

sequenceAlt :: (Alternative f) => [f a] -> f a
sequenceAlt xs = foldr1 (<|>) xs

-- consumes one token, regardless of value
get :: Parser t t
get = Parser $ \ts -> case ts of
    [] -> Nothing
    (x:xs) -> return (x,xs)

-- returns rest of tokens, consumes none
look :: Parser t [t]
look = Parser $ \ts -> return (ts, ts)

-- parser to match a single input token
token :: (Eq t) => t -> Parser t t
token t = Parser $ \ts -> case ts of
    [] -> Nothing
    (x:xs) -> if x == t then return (x, xs) else Nothing

char :: Char -> Parser Char Char
char = token

tokens :: (Eq t) => [t] -> Parser t [t]
tokens ts = sequence_ (map token ts) >> return ts

string :: String -> Parser Char String
string = tokens

satisfy :: (t -> Bool) -> Parser t t
satisfy p = Parser $ \ts -> case ts of
    []     -> Nothing
    (x:xs) -> if p x then return (x, xs) else Nothing

skipSpaces :: Parser Char ()
skipSpaces = (many (satisfy isSpace)) >> return ()

composeN :: Int -> (a -> a) -> a -> a
composeN n f x = foldr (\g y -> g y) x $ take n (repeat f)

data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Eq,Show)

type CATree = Tree Bool

data Direction = L | R
    deriving (Eq,Show)

type Binary = [Bool]
type Length = Int
type Rule = Binary
type Neighborhood = Binary
type Query = (Int, [Direction])
type ParseObject = (Rule, CATree, [Query])

repr :: CATree -> String
repr Empty = []
repr (Node x l r) = case (l,r) of
    (Empty,Empty) -> display x
    _ -> "(" ++ repr l ++ " " ++ display x ++ " " ++ repr r ++ ")"

treeSteps :: Rule -> Int -> String -> String
treeSteps r n = repr . caUpdates n r . parseTree

tester :: String
tester = unlines [treeSteps rule i treeStr | i <- [0..4]]
    where
        rule = toBinary 42354
        treeStr = "((. X (. . .)) . (X . (. X X)))"


parseTree :: String -> CATree
parseTree = fromMaybe . evalParser treeParser
    where fromMaybe (Just x) = x

main :: IO ()
main = interact solution

test :: IO ()
test = do
    s <- readFile "test.in"
    putStr $ solution s

solution :: String -> String
solution s = case tryRun s of
    Nothing -> error "Parse/evaluation error"
    Just res -> res

tryRun :: String -> Maybe String
tryRun s = do
    (rule,t,qs) <- evalParser parser s
    results <- sequence $ map (\(n,ds) -> navigateVal ds (caUpdates n rule t)) (resolveUpdateSteps qs)
    return $ unlines $ map display results
    where
        resolveUpdateSteps :: [(Int, [Direction])] -> [(Int, [Direction])]
        resolveUpdateSteps qs = zip (f 0 (map fst qs)) (map snd qs)
        f :: Int -> [Int] -> [Int]
        f acc [] = []
        f acc (n:ns) = let acc' = n + acc in acc' : f acc' ns

display :: Bool -> String
display False = "."
display True = "X"

navigate :: [Direction] -> Tree a -> Maybe (Tree a)
navigate [] t = return t
navigate (d:ds) Empty = Nothing
navigate (d:ds) (Node x l r) = navigate ds $ case d of
    L -> l
    R -> r

navigateVal :: [Direction] -> Tree a -> Maybe a
navigateVal ds t = navigate ds t >>= \(Node x l r) -> return x

ruleSize :: Int
ruleSize = 16

nextVal :: Rule -> Neighborhood -> Bool
nextVal r n = r !! (ruleSize - 1 - fromBinary n)

caVal :: CATree -> Bool
caVal Empty = False
caVal (Node x _ _) = x

caUpdates :: Int -> Rule -> CATree -> CATree
caUpdates n rule = composeN n (caUpdate rule)

caUpdate :: Rule -> CATree -> CATree
caUpdate = caUpdate' False

caUpdate' :: Bool -> Rule -> CATree -> CATree
caUpdate' _ _ Empty = Empty
caUpdate' p rule (Node x l r) = Node (rule !! ruleIndex) (caUpdate' x rule l) (caUpdate' x rule r)
    where
        neighborhood = [p, caVal l, x, caVal r]
        ruleIndex = ruleSize - 1 - fromBinary neighborhood

parser :: Parser Char ParseObject
parser = do
    ruleInt <- intParser
    skipSpaces
    tree <- treeParser
    skipSpaces
    intParser
    skipSpaces
    queries <- some queryParser
    return (mkRule ruleInt, tree, queries)

treeParser :: Parser Char CATree
treeParser = skipSpaces *> (leafParser <|> recurseParser) <* skipSpaces
    where
        leafParser = do
            b <- element
            return $ Node b Empty Empty
        element = do
            c <- char '.' <|> char 'X'
            return $ if c == '.' then False else True
        recurseParser = do
            char '('
            l <- treeParser
            b <- element
            r <- treeParser
            char ')'
            return $ Node b l r

stepsParser :: Parser Char [Direction]
stepsParser = char '[' *> many stepParser <* char ']'
    where
        stepParser = (char '>' >> return R) <|> (char '<' >> return L)

intParser :: Parser Char Int
intParser = pos <|> do
    char '-'
    n <- pos
    return $ (-1) * n
    where
        pos = some (satisfy isDigit) >>= \s -> return (read s :: Int)

queryParser :: Parser Char Query
queryParser = do
    i <- intParser
    skipSpaces
    steps <- stepsParser
    skipSpaces
    return (i, steps)


toBinary :: Int -> Binary
toBinary n = reverse $ map (\i -> i `elem` trueIndeces) [0..maxIndex]
    where
        maxIndex = foldr max 0 trueIndeces
        trueIndeces = go n
        go n | (n == 0) = []
             | (n == 1) = [0]
             | (n > 1) =
                 let i = log2 n
                 in i : go (n - 2^i)

log2 :: Int -> Int
log2 n = if n > 0 then go 0 else error "undefined log argument"
    where
        go i = if 2 ^ i <= n 
                then go (i + 1)
                else i - 1

fromBinary :: Binary -> Int
fromBinary b = sum $ map f $ zip (countdown (length b - 1)) $ map toZeroOne b
    where f (i,x) = 2^i * x

countdown :: Int -> [Int]
countdown 0 = [0]
countdown n = n : countdown (n-1)

toZeroOne :: Bool -> Int
toZeroOne True = 1
toZeroOne False = 0

zeroExtend :: Length -> Binary -> Binary
zeroExtend l b = (take (l - length b) (repeat False)) ++ b

mkRule :: Int -> Rule
mkRule = zeroExtend ruleSize . toBinary

