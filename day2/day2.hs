import Data.List ((\\))
import qualified Data.List as L

main :: IO ()
main = do
    inputRaw <- readFile "input.txt"
    let input = processInput inputRaw
    putStrLn $ "safe: " ++ show (count isSafe input)
    putStrLn $ "safe (tolerant): " ++ show (count isSafeTolerant input)
    -- mapM_ print (filter isSafeTolerant input \\ filter isSafe input)
    -- mapM_ print (filter (not . isSafeTolerant) input)

processInput :: String -> [Report]
processInput
    = map (map read . words) . lines

type Report = [Integer]

isSafe :: Report -> Bool
isSafe report = go (signum $ head report - last report) report
  where
    go :: Integer -> Report -> Bool
    go _ []  = False
    go _ [_] = True
    go s (x:xs)
        | abs (x - head xs) `notElem` [1..3] = False
        | signum (x - head xs) /= s          = False
        | otherwise                          = go s xs

isSafeTolerant :: Report -> Bool
isSafeTolerant x
    = any isSafe
    $ zipWith (++) (L.inits x) (map (drop 1) $ L.tails x)

count :: Eq a => (a -> Bool) -> [a] -> Integer
count p = foldr (\y acc -> if p y then acc + 1 else acc) 0
