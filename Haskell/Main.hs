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
outer f l1 l2 = (fmap . fmap) (uncurry f) (combinations l1 l2)

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

identity :: Num a => Int -> [[a]]
identity dimensions = [[fromIntegral $ fromEnum $ i == j | j <- [0 .. l]] | i <- [0 .. l]]
    where l = dimensions - 1

identity2 :: Num a => Int -> a -> [[a]]
identity2 dimensions modulus = (map . map) (* (modulus - 1)) (identity dimensions)

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ [] = []
removeItem x (y:ys) | x == y = removeItem x ys
                    | otherwise = y : removeItem x ys

groupNub :: Real a => a -> [[[a]]] -> [[[a]]]
groupNub modulus group = foldl check group group
    where i = identity2 (length (group !! 0)) modulus
          check a b = if e b `elem` a then removeItem b a else a
          e = matMultMod modulus i

cayleyProducts :: Int -> [(Int, Int)]
cayleyProducts n = [(i, j) | i <- [0 .. n], j <- [0 .. n]]

filterProducts :: ((Int, Int) -> Bool) -> [(Int, Int)] -> [(Int, Int)]
filterProducts f products = nub $ so <$> filter f products
    where so (a, b) = if a > b then (b, a) else (a, b)

getCommutatives :: [[Int]] -> [(Int, Int)] -> [(Int, Int)]
getCommutatives cayley products = filterProducts check products
    where check (i, j) = cayley !! i !! j == cayley !! j !! i

getInverses :: [[Int]] -> [(Int, Int)] -> Int -> [(Int, Int)]
getInverses cayley products identityIndex = filterProducts check products
    where check (i, j) = cayley !! i !! j == identityIndex

identity2Index :: Real a => [[[a]]] -> a -> Int
identity2Index group modulus = fromMaybe (-1) $ identity2 (length (group !! 0)) modulus `elemIndex` group

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
  
    let group = groupNub modulus $ generateGroup modulus initials
    let cayley = cayleyTable modulus group
    let iden2 = identity2Index group modulus
    let products = cayleyProducts $ length cayley - 1
    let commutative = getCommutatives cayley products
    let inverses = getInverses cayley products iden2

    print $ group
    print $ length group
    print $ length $ commutative
    print $ length $ inverses
    
    return ()
