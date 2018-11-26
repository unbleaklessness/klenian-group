module GroupAnalysis.Functions ( converge
                               , fixedPoint
                               , combinations
                               , outer
                               , getSecondFromPairs
                               ) where

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y = y
    | otherwise = converge p ys

fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f x = converge (==) $ iterate f x

combinations :: [a] -> [b] -> [[(a, b)]]
combinations l1 l2 = [[(a, b) | a <- l1] | b <- l2]

outer :: (a -> b -> c) -> [a] -> [b] -> [[c]]
outer f l1 l2 = (map . map) (uncurry f) (combinations l1 l2)

getSecondFromPairs :: Eq a => [(a, b)] -> a -> b
getSecondFromPairs pairs first = snd $ head $ filter (\ (a, b) -> a == first) pairs
