module Main where

import Data.List (elemIndex, elem)
import Data.Maybe (fromMaybe)

import GroupAnalysis.Matrix (identity, matMultMod, matPowMod, matScalarMult)
import GroupAnalysis.Cayley (cayleyTable, getCommutatives, getInverses, getEquivalents)
import GroupAnalysis.Generator (generateGroup)
import GroupAnalysis.Functions (getSecondFromPairs)
import GroupAnalysis.List (removeIndexes, removeIndexes2)

-- | TODO:
-- | - Sylov subgroups.
-- | - Center.
-- | - Normalizer.
-- | - Orders.

printInfo :: Show a => String -> [a] -> IO ()
printInfo name list = print $ name ++ ", " ++ show (length list) ++ " elements: " ++ show list

-- Multiplication of matrix M1 by M2 is: group !! (cayleyUnique !! m2index !! m1index).
main :: IO ()
main = do
    let modulus = 7
    let matMultMod' = matMultMod modulus
    let matPowMod' = matPowMod modulus

    let initials = [[[0, 3], [2, 4]], [[0, 1], [6, 0]], [[1, 1], [0, 1]], [[3, 0], [0, 5]]] -- MOD 7

    -- let m1 = [[0, 3], [2, 4]]
    -- let m2 = [[0, 1], [6, 0]]
    -- let m3 = m2 `matMultMod'` (m1 `matPowMod'` 2) `matMultMod'` m2
    -- let initials = [m1, m3] -- MOD 7

    -- let m1 = [[0, 3], [2, 4]]
    -- let m2 = [[0, 1], [6, 0]]
    -- let m3 = m2 `matMultMod'` m1 `matMultMod'` m2
    -- let initials = [m1, m3] -- MOD 7

    -- let initials = [[[2, 0], [0, 7]], [[0, 5], [5, 3]], [[1, 1], [0, 1]], [[0, 1], [12, 0]], [[2, 2], [3, 10]]] -- MOD 13
  
    let group = generateGroup modulus initials
    let cayley = cayleyTable modulus group
    let dimensions = length $ group !! 0
    let e1 = fromMaybe (-1) $ identity dimensions `elemIndex` group
    let e2 = fromMaybe (-1) $ identity dimensions `matScalarMult` (modulus - 1) `elemIndex` group
    let equivalents = getEquivalents cayley e2
    let groupUnique = removeIndexes equivalents group
    let cayleyUnique = removeIndexes2 equivalents cayley
    let commutatives = getCommutatives cayley
    let inverses = getInverses cayley e1

    -- printInfo "Group" group
    -- printInfo "Unique group" groupUnique
    -- printInfo "Cayley" cayley
    -- printInfo "Unique cayley" cayleyUnique
    -- printInfo "Inverses" inverses
    -- printInfo "Commutatives" commutatives

    print $ ">>> INVERSES >>>"
    let m1 = 111
    let i1 = getSecondFromPairs inverses m1
    print $ group !! (cayley !! i1 !! m1)
    print $ "<<< INVERSES <<<"

    print $ ">>> EQUIVALENTS MULTIPLICATIONS >>>"
    let m1 = 76
    let m2 = 99
    let i1 = cayley !! (cayley !! m2 !! m1) !! e2
    let i2 = cayley !! m2 !! m1
    let g = cayleyUnique !! 0
    print $ i1
    print $ i2
    print $ elemIndex i1 g
    print $ elemIndex i2 g
    print $ "<<< EQUIVALENTS MULTIPLICATIONS <<<"
    
    return ()



