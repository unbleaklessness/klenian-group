module Main where

import Data.List (union, permutations)

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y = y
    | otherwise = converge p ys

fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f x = converge (==) $ iterate f x

combinations :: [a] -> [b] -> [[(a, b)]]
combinations l1 l2 = map (\a -> map (\b -> (a, b)) l2) l1

outer :: ((a, b) -> (c, d)) -> [a] -> [b] -> [[(c, d)]]
outer f l1 l2 = map (\a -> map (\b -> f b) a) $ combinations l1 l2

-- generator l = 

main :: IO ()
main = do
    print $ fixedPoint (\x -> (x + 2 / x) / 2) 1
    print $ map (\a -> take 2 a) $ permutations [1, 2, 3]
    print $ outer (\(a, b) -> (succ a, pred b)) ['a', 'b', 'c'] [1, 2, 3]
    print "test" 
