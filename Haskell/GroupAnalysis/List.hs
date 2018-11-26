module GroupAnalysis.List ( removeItem
                          , removeIndex
                          , removeIndexes
                          , removeIndexes2
                          ) where

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ [] = []
removeItem x (y:ys) | x == y = removeItem x ys
                    | otherwise = y : removeItem x ys

removeIndex :: Int -> [a] -> [a]
removeIndex _ [] = []
removeIndex i (a:as) | i == 0 = as
                     | otherwise = a : removeIndex (i - 1) as

removeIndexes :: [Int] -> [a] -> [a]
removeIndexes indexes list = foldl (flip removeIndex) list indexes

removeIndexes2 :: [Int] -> [[a]] -> [[a]]
removeIndexes2 indexes table = removeIndexes indexes $ map (removeIndexes indexes) table
