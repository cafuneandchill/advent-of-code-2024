import qualified Data.List as L

main :: IO ()
main = do
    inputRaw <- readFile "input.txt"
    let input = processInput inputRaw
    let dist = uncurry distance input
    let sim = uncurry similarity input
    putStrLn $ "distance: " ++ show dist
    putStrLn $ "similarity: " ++ show sim

processInput :: String -> ([Integer], [Integer])
processInput
    = (\ [xs, ys] -> (xs, ys))
    . map (map read)
    . L.transpose
    . map words
    . lines

distance :: [Integer] -> [Integer] -> Integer
distance m n = go (L.sort m) (L.sort n)
  where
    go [] [] = 0
    go (x:xs) (y:ys) = abs (x - y) + go xs ys

similarity :: [Integer] -> [Integer] -> Integer
similarity [] _ = 0
similarity (x:xs) ys = x * count (== x) ys + similarity xs ys

count :: Eq a => (a -> Bool) -> [a] -> Integer
count p = foldr (\y acc -> if p y then acc + 1 else acc) 0