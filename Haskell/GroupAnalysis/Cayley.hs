module GroupAnalysis.Cayley ( cayleyTable
                            , getCommutatives
                            , getInverses
                            , getEquivalents
                            ) where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

import GroupAnalysis.Matrix (matMultMod)
import GroupAnalysis.Functions (outer)

cayleyTable :: Real a => a -> [[[a]]] -> [[Int]]
cayleyTable modulus group = (map . map) (fromMaybe (-1) . flip elemIndex group) products
    where products = outer (matMultMod modulus) group group

filterCayley :: (Int -> Int -> Bool) -> [[Int]] -> [(Int, Int)]
filterCayley p cayley = filter (/= (-1, -1)) [if p i j then (i, j) else (-1, -1) | j <- [0 .. l], i <- [0 .. l]]
    where l = length cayley - 1

getEquivalents :: [[Int]] -> Int -> [Int]
getEquivalents cayley idIndex = foldl check [] [0 .. length cayley - 1]
    where check a b = if cayley !! idIndex !! b `elem` a then a else b : a

getCommutatives :: [[Int]] -> [(Int, Int)]
getCommutatives cayley = filterCayley check cayley
    where check i j = cayley !! i !! j == cayley !! j !! i

getInverses :: [[Int]] -> Int -> [(Int, Int)]
getInverses cayley idIndex = filterCayley check cayley
    where check i j = cayley !! i !! j == idIndex
