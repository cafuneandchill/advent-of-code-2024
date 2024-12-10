module Utils where

count :: Eq a => (a -> Bool) -> [a] -> Integer
count p = foldr (\y acc -> if p y then acc + 1 else acc) 0