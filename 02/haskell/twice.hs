import System.Environment
import Data.HashSet
import Data.Hashable

readInt :: String -> IO Int
readInt (c:cs) | c == '+' = readInt cs
readInt cs = readIO cs

partialSums :: [Int] -> [Int]
partialSums = partialSums' 0 where
    partialSums' n [] = [n]
    partialSums' n (x:xs) = [n] ++ (partialSums' (n+x) xs)

reachedTwice :: (Eq a, Hashable a) => [a] -> a
reachedTwice = go empty where
    go _ [] = error "No duplicate element found"
    go hs (y:ys) | y `member` hs = y
    go hs (y:ys) = go (insert y hs)  ys
    
main :: IO ()
main = do
    args <- getArgs
    contents <- readFile $ args !! 0
    numbers <- traverse readInt $ lines contents
    let partials = partialSums $ cycle numbers
    print $ reachedTwice partials
