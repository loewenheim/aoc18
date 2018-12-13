import System.Environment

readInt :: String -> IO Int
readInt (c:cs) | c == '+' = readInt cs
readInt cs = readIO cs

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile $ args !! 0
    numbers <- traverse readInt $ lines contents
    print $ sum numbers
