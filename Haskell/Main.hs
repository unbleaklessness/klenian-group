module Main where

import Data.List (transpose, nub)
import Data.Fixed (mod')

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y = y
    | otherwise = converge p ys

fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f x = converge (==) $ iterate f x

combinations :: [a] -> [b] -> [[(a, b)]]
combinations l1 l2 = (\ a -> (\ b -> (a, b)) <$> l2) <$> l1

outer :: (a -> b -> c) -> [a] -> [b] -> [[c]]
outer f l1 l2 = (\ a -> (\ b -> uncurry f b) <$> a) <$> combinations l1 l2

matMult :: Num a => [[a]] -> [[a]] -> [[a]]
matMult a b = [[sum $ zipWith (*) ar bc | bc <- (transpose b)] | ar <- a]

matMultMod :: Real a => a -> [[a]] -> [[a]] -> [[a]]
matMultMod modulus a b = (map . map) (flip mod' modulus) (matMult a b)

generateGroup :: Real a => a -> [[[a]]] -> [[[a]]]
generateGroup modulus = fixedPoint (\ a -> nub $ concat $ outer (\ x y -> matMultMod modulus x y) a a)

main :: IO ()
main = do
    let initials = [[[0, 3], [2, 4]], [[0, 1], [6, 0]], [[1, 1], [0, 1]], [[3, 0], [0, 5]]]
    let group = generateGroup 7 initials
    
    print $ group
    print $ length group

    return ()
