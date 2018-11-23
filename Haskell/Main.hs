module Main where

import Data.List (transpose, nub, elemIndex, elem)
import Data.Fixed (mod')
import Data.Maybe (fromMaybe)

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

matMult :: Num a => [[a]] -> [[a]] -> [[a]]
matMult a b = [[sum $ zipWith (*) ar bc | bc <- (transpose b)] | ar <- a]

matMultMod :: Real a => a -> [[a]] -> [[a]] -> [[a]]
matMultMod modulus a b = (map . map) (flip mod' modulus) (matMult a b)

matPow :: Num a => [[a]] -> Int -> [[a]]
matPow m p = iterate (matMult m) m !! max 0 p

generateGroup :: Real a => a -> [[[a]]] -> [[[a]]]
generateGroup modulus = fixedPoint (\ a -> nub $ concat $ outer (matMultMod modulus) a a)

cayleyTable :: Real a => a -> [[[a]]] -> [[Int]]
cayleyTable modulus group = (map . map) (fromMaybe (-1) . flip elemIndex group) products
    where products = outer (matMultMod modulus) group group

identity1 :: Num a => Int -> [[a]]
identity1 dimensions = [[fromIntegral $ fromEnum $ i == j | j <- [0 .. l]] | i <- [0 .. l]]
    where l = dimensions - 1

identity2 :: Num a => Int -> a -> [[a]]
identity2 dimensions modulus = (map . map) (* (modulus - 1)) (identity1 dimensions)

identity1Index :: Real a => [[[a]]] -> Int
identity1Index group = fromMaybe (-1) $ identity1 (length (group !! 0)) `elemIndex` group

identity2Index :: Real a => a -> [[[a]]] -> Int
identity2Index modulus group = fromMaybe (-1) $ identity2 (length (group !! 0)) modulus `elemIndex` group

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

equivalentIndexes :: [[Int]] -> Int -> [Int]
equivalentIndexes cayley idIndex = foldl check [] [0 .. length cayley - 1]
    where check a b = if cayley !! idIndex !! b `elem` a then a else b : a

pairs :: Int -> [(Int, Int)]
pairs n = [(i, j) | i <- [0 .. n], j <- [0 .. n]]

filterPairs :: ((Int, Int) -> Bool) -> [(Int, Int)] -> [(Int, Int)]
filterPairs f pairs = nub $ so <$> filter f pairs
    where so (a, b) = if a > b then (b, a) else (a, b)

getCommutatives :: [[Int]] -> [(Int, Int)] -> [(Int, Int)]
getCommutatives cayley pairs = filterPairs check pairs
    where check (i, j) = cayley !! i !! j == cayley !! j !! i

getInverses :: [[Int]] -> [(Int, Int)] -> Int -> [(Int, Int)]
getInverses cayley pairs idIndex = filterPairs check pairs
    where check (i, j) = cayley !! i !! j == idIndex

-- Multiplication of matrix M1 by M2 is: group !! (cayleyUnique !! m2index !! m1index).
main :: IO ()
main = do
    let initials = [[[0, 3], [2, 4]], [[0, 1], [6, 0]], [[1, 1], [0, 1]], [[3, 0], [0, 5]]]
    let modulus = 7

    -- let m1 = [[0, 3], [2, 4]]
    -- let m2 = [[0, 1], [6, 0]]
    -- let m3 = m2 `matMult` (m1 `matPow` 2) `matMult` m2
    -- let initials = [m1, m3]
    -- let modulus = 7

    -- let initials = [[[2, 0], [0, 7]], [[0, 5], [5, 3]], [[1, 1], [0, 1]], [[0, 1], [12, 0]], [[2, 2], [3, 10]]]
    -- let modulus = 12
  
    let group = generateGroup modulus initials
    let cayley = cayleyTable modulus group
    let iden1 = identity1Index group
    let iden2 = identity2Index modulus group
    let eqIndexes = equivalentIndexes cayley iden2
    let groupUnique = removeIndexes eqIndexes group
    let cayleyUnique = removeIndexes2 eqIndexes cayley
    let multPairs = pairs $ length groupUnique - 1
    -- let multPairs = pairs $ length group - 1
    let commutatives = getCommutatives cayleyUnique multPairs
    -- let commutatives = getCommutatives cayley multPairs
    let inverses = getInverses cayleyUnique multPairs iden1
    -- let inverses = getInverses cayley multPairs iden1

    print $ group
    print $ length group
    print $ groupUnique
    print $ length groupUnique
    -- print $ cayley
    -- print $ length cayley
    -- print $ cayleyUnique
    -- print $ length cayleyUnique
    print $ inverses
    print $ length inverses
    print $ commutatives
    print $ length commutatives

    print "=== TEST MULTIPLICATION ==="

    let i1 = 29
    let i2 = 76
    print $ matMultMod modulus (groupUnique !! i1) (groupUnique !! i2)
    print $ group !! (cayleyUnique !! i2 !! i1)

    return ()


